import re, os, tqdm
from src.smile_ks_dependency_tree_eval.dep_tree_fix_listener import DepTreeFix, DepTreeFix3, DepTreeFix5, Text, Trace, Ks, KSAR, Hypothesis, Program, Dep
from src.smile_ks_dependency_tree_eval.utils import add_ks
from owlready2 import default_world, ObjectProperty, DataProperty, rdfs, Thing 
from py2graphdb.config import config as CONFIG
from py2graphdb.utils.db_utils import resolve_nm_for_dict, PropertyList
from py2graphdb.ontology.operators import *
from smile_base.utils import init_db

if not os.path.exists(CONFIG.LOG_DIR):
    os.makedirs(CONFIG.LOG_DIR)

def parse(sent):
    annotation = dict()
    ann = nlp_parser.parse(sent)
    annotation["Dep"] = nlp_parser.resolved_to_triples(ann)
    annotation["Word and Pos"] = nlp_parser.get_words(ann)
    return annotation


def gen_ksar(inputs: list[Dep], trace:Trace, fix_num: int):
    input_klasses = [hypo.klass for hypo in inputs]
    ks = Ks.search(props={smile.hasPyName:py_name, hasonly(smile.hasInputDataLevels):input_klasses, hasonly(smile.hasOutputDataLevels):output.klass}, how='first')[0]
    ks_ar = KSAR()
    ks_ar.keep_db_in_synch = False
    ks_ar.ks = ks.id
    ks_ar.trace = trace.id
    ks_ar.cycle = 0
    for hypo in inputs:
        ks_ar.input_hypotheses = hypo.id
        hypo.for_ks_ars = ks_ar.inst_id

    ks_ar.save()
    ks_ar.keep_db_in_synch = True
    return ks_ar


smile = default_world.get_ontology(CONFIG.NM)
with smile:    
    init_db.init_db()
    add_ks.add_ks()
    init_db.load_owl('./ontology_cache/cids.ttl')

    # trace = Trace(keep_db_in_synch=True)

    text = "The school insisted on providing documents."
    annotation = parse(text)

    hold_deps = []
    rel_word_queries = {}
    if annotation["Word and Pos"] is not None:
        for token in annotation["Word and Pos"]:
            word = Word.find_generate(trace_id=trace.id, content=token["content"], content_label = token["content_label"], start=token["start"], end=token["end"])
            word.save()
            rel_word_queries[token["content_label"]] = word

            pos = Pos.find_generate(word_ids=[word.id], pos_tag=token["pos"],trace_id=trace.id)
            pos.save()
            word.pos = pos.inst_id

    if annotation["Dep"] is not None:
        for v in annotation["Dep"]:
            dep = v[1]
            subject_content_label = v[0][0]
            object_content_label = v[2][0]
            subject_w, subject_i = re.findall(r'(.+)\-([0-9]+)', subject_content_label)[:2][0]

            if subject_content_label not in rel_word_queries.keys():
                continue
            subject_word_id = rel_word_queries[subject_content_label].id
            
            object_w, object_i = re.findall(r'(.+)\-([0-9]+)', object_content_label)[:2][0]
            if object_content_label not in rel_word_queries.keys():
                continue
            object_word_id = rel_word_queries[object_content_label].id

            dep = Dep.find_generate(dep=dep, subject_id=subject_word_id, object_id=object_word_id,trace_id=trace.id)
            dep.save()
            hold_deps.append(dep)

    ks_ar = gen_ksar(inputs=hold_deps, trace=trace, fix_num = 5)
    ks_ar.ks_status=0
    ks_ar.save()

    ks_ar_5 = DepTreeFix.process_ks_ars(loop=False)
    
    ks_ar_5_loaded = [ks_ar_5.load() for k in ks_ar_5]

    outs = [Hypothesis(inst_id=hypo_id).cast_to_graph_type() for hypo_id in ks_ar.hypotheses]
    for out in outs:
        try:
            print(out, out.content)
            for concept_id in out.concepts:
                concept = Hypothesis(concept_id).cast_to_graph_type()
                print("\t", concept.certainty, concept.klass)
        except:
            pass
