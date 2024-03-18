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

def gen_ksar(inputs:list, output:Hypothesis, py_name:str, trace:Trace):
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

    # text = "Mary hugged the boy the girl left."

    # hold_deps = []
    # for dep in deps
    #     dep = Dep.find_generate(content=text, trace_id=trace.id)
    #     dep.save()
    #     hold_deps.append(dep)
    # ks_ar = gen_ksar(inputs=[Dep], output=Program, trace=trace)
    # ks_ar.ks_status=0
    # ks_ar.save()

    
    # ks_ar_3 = DepTreeFix.process_ks_ars(loop=False)
    # ks_ar_3.load()
    # outs = [Hypothesis(inst_id=hypo_id).cast_to_graph_type() for hypo_id in ks_ar.hypotheses]
    # for out in outs:
    #     try:
    #         print(out, out.content)
    #         for concept_id in out.concepts:
    #             concept = Hypothesis(concept_id).cast_to_graph_type()
    #             print("\t", concept.certainty, concept.klass)
    #     except:
    #         pass