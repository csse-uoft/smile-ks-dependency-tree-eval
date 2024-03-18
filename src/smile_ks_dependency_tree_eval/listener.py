import re, os, tqdm
from owlready2 import default_world, onto_path, ObjectProperty, DataProperty, rdfs, Thing 
onto_path.append('./ontology_cache/')
from py2graphdb.config import config as CONFIG
smile = default_world.get_ontology(CONFIG.NM)
with smile:
    from py2graphdb.Models.graph_node import GraphNode, SPARQLDict
    from py2graphdb.utils.db_utils import resolve_nm_for_dict, PropertyList
    from smile_base.Model.knowledge_source.knowledge_source import KnowledgeSource
    from smile_ks_dependency_tree_eval.libs import nlp_parser
    from src.smile_ks_dependency_tree_eval.utils import add_ks

    from smile_base.Model.data_level.phrase import Phrase
    from smile_base.Model.data_level.text import Text
    from smile_base.Model.data_level.sentence import Sentence
    from smile_base.Model.data_level.hypothesis import Hypothesis
    from smile_base.Model.controller.ks import Ks
    from smile_base.Model.controller.ks_ar import KSAR
    from smile_base.Model.controller.trace import Trace

from py2graphdb.ontology.operators import *

# from nltk.tokenize import sent_tokenize, word_tokenize
from nltk.tokenize.punkt import PunktSentenceTokenizer
import time
from py2graphdb.utils.misc_lib import *

import nltk
from nltk.stem.wordnet import WordNetLemmatizer
nltk.download('omw-1.4')
wnl = WordNetLemmatizer()




class DepTreeFix(KnowledgeSource):
    """
    A knowledge source class that processes QA1 Ner

    Attributes
    ----------
    description: str
        String of description to be parsed
    annotation: Dict
        Formatted annotation for each task
    corenlp_output: Dict
        Annotated output of StanfordCoreNLP parser
    """

    CC_WORDS = []
    with open("knowledge_sources/dep_tree_fix/data/cc_words.txt", "r") as f:
        for line in f:
            CC_WORDS.append(line.strip().split(" "))

    FIX_NUM_COMMANDS = {
        3: "findall([W,D],(spo(D,W)),L),writeln(L)",
        5: "findall([W],(fix5_pattern(W)),L),writeln(L)"
    }

    # TODO: change it back after testing it locally
    def __init__(self, hypothesis_ids=None, ks_ar=None, trace=None):
        # fields = [v for v in Ks.ALL_KS_FORMATS.values() if v[0] == self.__class__.__name__][0]
        # super().__init__(fields[1], fields[2], fields[3], trace, hypothesis_ids, ks_ar)
        self.ks_ar = ks_ar
        self.trace = trace
        self.hypothesis_ids = hypothesis_ids
        
        self.dep_hypos          = []
        self.dep_triples        = []
        self.description        = None
        self.output_hypos       = []

    @classmethod
    def process_ks_ars(cls, loop=True):
        """
        A class method that processes all the ks_ars with py_name='ParseTokenize' and status=0.

        :param cls: The class itself (implicit parameter).
        :type cls: type
        :return: None
        """
        while True:        
            for fix_num in cls.FIX_NUM_COMMANDS.keys():                    
                ks = Ks.search(props={smile.hasPyName: f'DepTreeFix{fix_num}'}, how='first')
                            
                if len(ks) >0:
                    ks = ks[0]
                else:
                    continue

                ks_ar = KSAR.search(props={smile.hasKS:ks.id, smile.hasKSARStatus:0}, how='first')
                
                if len(ks_ar) > 0:
                    ks_ar = ks_ar[0]
                    cls.logger(trace_id=ks_ar.trace, text=f"Processing ks_ar with id: {ks_ar.id}")

                    # Get the hypothesis ids from the ks_ar
                    in_hypo_ids = ks_ar.input_hypotheses
                    in_hypos = [Hypothesis(inst_id=hypo_id).cast_to_graph_type() for hypo_id in in_hypo_ids]
                    in_hypos = [hypo for hypo in in_hypos if isinstance(hypo, Dep)]
                    hypo_ids = [hypo.id for hypo in in_hypos]

                    # Get the trace from the ks_ar
                    trace = Trace(inst_id=ks_ar.trace)
                    
                    # Construct an instance of the ks_object
                    ks_object = eval(f"{ks.py_name}(hypothesis_ids=hypo_ids, ks_ar=ks_ar, trace=trace)")
                    
                    # Call ks_object.set_input() with the necessary parameters
                    ks_ar.ks_status = 1
                    ks_object.set_input(dep_hypos=in_hypos)
                    
                    ks_ar.ks_status = 2               
                    hypotheses = ks_object.get_outputs()
                    for hypo in hypotheses:
                        ks_ar.hypotheses = hypo.id 
                        trace.hypotheses = hypo.id

                    # log output
                    LOG_FILE_TEMPLATE = CONFIG.LOG_DIR+'smile_trace_log.txt'
                    filename = LOG_FILE_TEMPLATE.replace('.txt', f"_{trace.id}.txt")
                    ks_ar.summary(filename=filename)

                    ks_ar.ks_status = 3
                if not loop:                        
                    return ks_ar
            time.sleep(1)  

    def set_input(self, dep_hypos: list["Dep"]):
        """Run corenlp parsing functions of the requested output dependency tree.

        :param data_levels: output data levels that the contoller requests.
                            default is set to all possible output levels.
                            it could be both a string or a list.
        :return: updated corenlp output
        """
        self.dep_hypos = dep_hypos
        self.set_dep()
        self.apply_fix()
        print(self.dep_triples) ##########
        print(self.description) #############
    
    @staticmethod
    def process_dep(dep: "Dep"): #[['hugged-2', 'VBD'], 'nsubj', ['Mary-1', 'NNP']]
        subject_word = dep.subject_word
        object_word = dep.object_word
        relationship = dep.dep
        
        subject_content = subject_word.content
        subject_label = subject_word.label
        subject_pos = subject_word.pos.tag
        object_content = object_word.content
        object_label = object_word.label
        object_pos = object_word.pos.tag

        triple = [[subject_label, subject_pos], relationship, [object_label, object_pos]]
        words = ((subject_label, subject_content), (object_label, object_content)) ## (('hugged-2','hugged'),('Mary-1', 'Mary'))
        return triple, words
    
    def set_dep(self) -> None: 
        # reconstruct and set dep triples
        triples = []
        word_dict = {}
        for dep in self.dep_hypos:
            triple, words = self.process_dep(dep) 
            triples.append(triple)
            
            for contentlabel, word in words:
                word_dict[contentlabel] = word

        self.dep_triples = triples 
        ## reconstruct scentence
        sorted_keys = sorted(word_dict.keys(), key=lambda x: int(x.split('-')[1]))
        sentence = ' '.join(word_dict[key] for key in sorted_keys if word_dict[key] != "ROOT")
        self.description = sentence

    def get_outputs(self):
        raise NotImplementedError
    
    def write_to_prolog_file(self):
        pl_dir = "pyscript/app/scripts/scroll/prolog/"
        
        data_dir = os.path.join(pl_dir, "data/")
        os.makedirs(data_dir, exist_ok=True)
        
        # create prolog file for the fix instance
        pl_file = os.path.join(data_dir, "temp.pl")
        if not os.path.exists(pl_file):
            os.system("touch %s" % (pl_file))

        # write to pl file
        with open(pl_file, "w") as f:
            res_content =  ":- style_check(-discontiguous).\n"
            res_content += f":- ensure_loaded(\"{pl_dir}fix{self.fix_num}\").\n"
            res_content += ":- dynamic coref/1.\n"
            res_content += ("\n".join(["gram(%s,\"%s\",%s,%s)."%(i,t,[l1,l2],[r1,r2]) for i,[[l1,l2],t,[r1,r2]] in enumerate(self.dep_triples)])).replace("['","[\"").replace("']","\"]").replace("',","\",").replace(", '", ", \"")
            f.write(res_content)
    
    @staticmethod
    def search_fix(search_cmd):
        pl_dir = "pyscript/app/scripts/scroll/prolog/"
        pl_file =  os.path.join(pl_dir, "data/temp.pl") # prolog source file
        service_file = os.path.join(pl_dir, "results.txt")
        cmd = "swipl -s %s -g \""%(pl_file) + search_cmd + ".\" -g halt > %s"%(service_file)
        _=os.system(cmd)

    @staticmethod
    def clean_prolog_files():
        # """Deletes all files that were generated by prolog commands"""
        pl_dir = "pyscript/app/scripts/scroll/prolog/"
        data_dir = os.path.join(pl_dir, "data/")
        service_file = os.path.join(pl_dir, "results.txt")
        if os.path.exists(data_dir): shutil.rmtree(data_dir)
        if os.path.exists(service_file):
            os.remove(service_file)
    
    def apply_fix(self):
        fix_num = self.fix_num
        
        pl_dir = "pyscript/app/scripts/scroll/prolog/"
        service_file = os.path.join(pl_dir, "results.txt")
        fix_needed = True
        while fix_needed:
            self.write_to_prolog_file()
            
            # fetch the fix method and command
            command = self.FIX_NUM_COMMANDS.get(fix_num, None)
            if command == None:
                raise ValueError(f"prolog pattern for fix_{fix_num} not found")  

            self.search_fix(command)
            
            with open(service_file, "r") as f:
                line = f.readline().strip()
            fix_applied = self.fix(line)
            print(f"fix applied: {fix_applied}")
            if not fix_applied:
                fix_needed = False
        self.clean_prolog_files()
    
    def fix(self, line):
        raise NotImplementedError
    

class DepTreeFix3(DepTreeFix):
    fix_num = 3

    # TODO: change initialization
    def __init__(self, hypothesis_ids=None, ks_ar=None, trace=None):
        # fields = [v for v in Ks.ALL_KS_FORMATS.values() if v[0] == self.__class__.__name__][0]
        super().__init__(trace, hypothesis_ids, ks_ar)

    def get_outputs(self):
        certainty = 1
        text_hypo = Text.find_generate(trace_id=self.trace.id, content=self.description, certainty=certainty)
        self.output_hypos.append(text_hypo)
        return self.output_hypos

    # TODO: for debugging purpose, delete this in final version
    def parse(self):
        sent = self.description
        ann = nlp_parser.parse(sent)
        triples = nlp_parser.resolved_to_triples(ann)
        self.dep_triples = triples

    def get_word_phrase(self, start_word):
        word_to_edges = {}
        for src, relation, tgt in self.dep_triples:
            src_word = src[0]
            tgt_word = tgt[0]
            if src_word not in word_to_edges:
                word_to_edges[src_word] = []
            word_to_edges[src_word].append((relation, tgt_word))

        # define helper dfs with cycle detection to do the graph traversal
        def dfs(word, phrase, visited):
            if word in visited: 
                return
            visited.add(word)
            phrase.append(word)
            for edge in word_to_edges.get(word, []):
                tgt_word = edge[1]
                dfs(tgt_word, phrase, visited)
            visited.remove(word)

        phrase = []
        visited = set()
        dfs(start_word, phrase, visited)
        phrase.sort(key=lambda x: int(x.split('-')[1]))
        return phrase

    def fix(self, fix_line):
        """
        """
        triple_elements = fix_line.split(",")
        triple_elements = [element.strip("[]") for element in triple_elements]
        # each prolog triple has 7 elements
        if len(triple_elements) <= 1:
            return False
        
        # the original word is whom
        word_to_insert = "whom"
        subject = triple_elements[1]
        # get the phrase corresponding to the subject word to decide the excact insert position
        phrase = self.get_word_phrase(subject)
        print(phrase)
        # insert before the first word in the phrase
        insert_pos = int(phrase[0].rsplit("-", 1)[1]) - 1
        sents = self.description.split()[:]
        print(f"Fix 3: {triple_elements[0]}| insert {word_to_insert}| before: {sents[insert_pos]}| into sentence: {self.description}\n")      
        sents.insert(insert_pos, word_to_insert)
        sentence = " ".join(sents)
        self.description = sentence
        self.parse()
        return True


class DepTreeFix5(DepTreeFix):
    fix_num = 5

    # TODO: change initialization
    def __init__(self, hypothesis_ids=None, ks_ar=None, trace=None):
        # fields = [v for v in Ks.ALL_KS_FORMATS.values() if v[0] == self.__class__.__name__][0]
        super().__init__(trace, hypothesis_ids, ks_ar)
    
    def get_outpus(self):
        # TODO: modify the implementation
        pass
        # # a dict to store all word that has been processed
        # processed_word = {}


        # # for tgt, _, src in self.dep_triples:
        # #     tgt_word = tgt[0]
        # #     src_word = src[0]
        # #     tgt_hypo = processed_word.get(tgt_word, None)
        # #     src_hypo = processed_word.get(src_word, None)
            
        # for token in self.annotation["Word and Pos"]:

        #     certainty = 1
        #     word = Word.find_generate(
        #         trace_id=self.trace.id,
        #         content=token["content"],
        #         content_label = token["content_label"],
        #         start=token["start"],
        #         end=token["end"],
        #         certainty=certainty)
        #     word.from_ks_ars = self.ks_ar.id
        #     self.store_hypotheses.append(word)
        #     rel_word_queries[token["content_label"]] = word

        #     certainty = 1
        #     pos = Pos.find_generate(word_ids=[word.id], pos_tag=token["pos"],trace_id=self.trace.id, certainty=certainty)
        #     pos.from_ks_ars = self.ks_ar.id
        #     word.pos = pos.inst_id
        #     self.store_hypotheses.append(pos)

        # for v in self.annotation["Dep"]:
        #     dep = v[1]
        #     subject_content_label = v[0][0]
        #     object_content_label = v[2][0]
        #     subject_w, subject_i = re.findall(r'(.+)\-([0-9]+)', subject_content_label)[:2][0]

        #     if subject_content_label not in rel_word_queries.keys():
        #         continue
        #     subject_word_id = rel_word_queries[subject_content_label].id
            
        #     object_w, object_i = re.findall(r'(.+)\-([0-9]+)', object_content_label)[:2][0]
        #     if object_content_label not in rel_word_queries.keys():
        #         continue
        #     object_word_id = rel_word_queries[object_content_label].id

        #     certainty = 1
        #     dep = Dep.find_generate(dep=dep, subject_id=subject_word_id, object_id=object_word_id,trace_id=self.trace.id, certainty=certainty)
        #     dep.from_ks_ars = self.ks_ar.id
        #     self.store_hypotheses.append(dep)

        # return self.dep_hypos

    def fix(self, fix_line):
        """
        """
        if fix_line == "[]" or not fix_line:
            return False
        
        # triple_element: [verb1-N, VB_, verb2-N, VBG, noun-N, __]
        triple_elements = fix_line.split(",")
        triple_elements = [element.strip("[]") for element in triple_elements]
        verb2 = triple_elements[2].rsplit("-", 1)[0].lower()
        noun = triple_elements[4].rsplit("-", 1)[0].lower()
        noun = wnl.lemmatize(noun)
        
        # detect if the checked verb and noun is actually a noun phrase, if so exclude
        if any(map(lambda phrase: phrase[0]==verb2 and phrase[1] in noun, self.CC_WORDS)):
            return False
        
        # find obl relation between verb1 and noun and swap noun with verb 2
        obl_rel = next(filter(lambda triple: triple[0][0] == triple_elements[0]
                        and "obl" in triple[1] and triple[2][0] == triple_elements[4], self.dep_triples))
        obl_rel[2] = [triple_elements[2], triple_elements[3]]
        # find obl relation between noun and verb2 and swap, replacing relation with obj
        amod_rel = next(filter(lambda triple: triple[0][0] == triple_elements[4]
                        and "amod" in triple[1] and triple[2][0] == triple_elements[2], self.dep_triples))
        amod_rel[0], amod_rel[2] = amod_rel[2], amod_rel[0]
        amod_rel[1] = "obj"
        
        print(f"Applied fix 5\n")
        return True
    

if __name__ == '__main__':
    print('DepTreeFix script started')
    add_ks.add_ks()

    with smile:
        DepTreeFix.process_ks_ars(loop=True)
