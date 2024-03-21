import os, shutil
from owlready2 import default_world, onto_path
onto_path.append('./ontology_cache/')
from py2graphdb.config import config as CONFIG
smile = default_world.get_ontology(CONFIG.NM)
with smile:
    from smile_base.Model.knowledge_source.knowledge_source import KnowledgeSource
    from smile_ks_dependency_tree_eval.utils import add_ks
    from smile_base.Model.data_level.word import Word
    from smile_base.Model.data_level.pos import Pos
    from smile_base.Model.data_level.dep import Dep
    from smile_base.Model.data_level.text import Text
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
    A knowledge source class that identifies and corrects structural errors in parsed dependency trees.

    Attributes
    -----------
    dep_hypos : list[Dep]
        A list of Dep data level objects representing the dependency hypotheses being processed.
    dep_triples : list[list]
        A list of dependency triples derived from the processed Dep objects.
    description : str
        A string of description reconstructed from the dependency triples.
    output_hypos : list[Hypothesis]
        A list of Output hypotheses generated by the knowledge source.
    """

    CC_WORDS = []
    with open("smile_ks_dependency_tree_eval/libs/scroll/data/cc_words.txt", "r") as f:
        for line in f:
            CC_WORDS.append(line.strip().split(" "))

    FIX_NUM_COMMANDS = {
        3: "findall([W,D],(spo(D,W)),L),writeln(L)",
        5: "findall([W],(fix5_pattern(W)),L),writeln(L)"
    }

    PL_DIR = "smile_ks_dependency_tree_eval/libs/scroll/prolog"

    def __init__(self, hypothesis_ids, ks_ar, trace):
        fields = [v for v in Ks.ALL_KS_FORMATS.values() if v[0] == self.__class__.__name__][0]
        super().__init__(fields[1], fields[2], fields[3], trace, hypothesis_ids, ks_ar)

        self.ks_ar = ks_ar
        self.trace = trace
        self.hypothesis_ids = hypothesis_ids
        
        self.fix_applied        = False
        self.dep_hypos          = []
        self.dep_triples        = []
        self.description        = None
        self.output_hypos       = []

    @classmethod
    def process_ks_ars(cls, loop=True):
        """
        A class method that processes all KSARs with a specific fix number. 
        It iterates through KSARs needing dependency tree fixes and applies the necessary corrections.

        :param cls: The class itself (implicit parameter).
        :type cls: DepTreeFix
        :param fix_num: The specific fix number to be applied during the DepTreeFix operation.
        :type fix_num: int
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

    def set_input(self, dep_hypos):
        """
        Sets the input dependency hypotheses for the DepTreeFix operation. 
        This method initializes the process of fixing the dependency tree based on the provided list of Dep objects.

        :param dep_hypos: A list of Dep objects representing the dependency hypotheses to be processed.
        :type dep_hypos: list[Dep]
        """

        self.dep_hypos = dep_hypos
        self.set_dep()
        self.apply_fix()
    
    @staticmethod
    def process_dep(dep):
        """
        Processes a single Dep object and constructs a triple representation along with the associated words.

        :param dep: A Dep object representing a dependency relation to be processed.
        :type dep: Dep
        :return: A tuple containing the processed triple and associated words.
        :rtype: (list, tuple)
        """
        subject_word = dep.subject_word
        object_word = dep.object_word
        relationship = dep.dep
        
        subject_content = subject_word.content
        subject_label = subject_word.content_label
        subject_pos = subject_word.pos.tag
        object_content = object_word.content
        object_label = object_word.content_label
        object_pos = object_word.pos.tag

        subject_str = f"{subject_content}-{subject_label}"
        object_str = f"{object_content}-{object_label}"
        triple = [[object_str, subject_pos], relationship, [object_str, object_pos]]
        words = ((subject_str, subject_content), (object_str, object_content)) ## (('hugged-2','hugged'),('Mary-1', 'Mary'))
        return triple, words
    
    def set_dep(self): 
        """
        Reconstructs dependency triples and the sentence description based on the current list of Dep objects.
        """
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
        sorted_keys = sorted(word_dict.keys(), key=lambda x: int(x.rsplit("-", 1)[1]))
        sentence = ' '.join(word_dict[key] for key in sorted_keys if word_dict[key] != "ROOT")
        self.description = sentence

    def get_outputs(self):
        raise NotImplementedError
    
    def write_to_prolog_file(self):
        """
        Writes the current state of dependency triples to a Prolog file for further processing and fix application.
        """
        pl_dir = self.PL_DIR
        
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
    
    @classmethod
    def search_fix(cls, search_cmd):
        """
        Executes Prolog command to search for potential fixes in the dependency tree.

        :param search_cmd: The Prolog command to be executed.
        :type search_cmd: str
        """
        pl_dir = cls.PL_DIR
        pl_file =  os.path.join(pl_dir, "data/temp.pl") # prolog source file
        service_file = os.path.join(pl_dir, "results.txt")
        cmd = "swipl -s %s -g \""%(pl_file) + search_cmd + ".\" -g halt > %s"%(service_file)
        _=os.system(cmd)

    @classmethod
    def clean_prolog_files(cls):
        """
        Cleans up Prolog files generated during the fix application process.
        """
        pl_dir = cls.PL_DIR
        data_dir = os.path.join(pl_dir, "data/")
        service_file = os.path.join(pl_dir, "results.txt")
        if os.path.exists(data_dir): shutil.rmtree(data_dir)
        if os.path.exists(service_file):
            os.remove(service_file)
    
    def apply_fix(self):
        """
        Applies the specified fix to the dependency tree by writing to Prolog files, executing Prolog commands, and processing the results.
        """
        fix_num = self.fix_num
        
        pl_dir = self.PL_DIR
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
            if fix_applied:
                self.fix_applied = True
            else:
                fix_needed = False
        self.clean_prolog_files()
    
    def fix(self, line):
        raise NotImplementedError
    

class DepTreeFix3(DepTreeFix):
    fix_num = 3

    def __init__(self, hypothesis_ids, ks_ar, trace):
        super().__init__(trace, hypothesis_ids, ks_ar)

    def get_outputs(self):
        """
        Retrieves the processed dependency hypotheses after applying fix 3. This implementation generates a Text hypothesis based on the modified description.

        :return: A list containing the generated Text hypothesis after fix application.
        :rtype: list[Text]
        """
        if self.fix_applied:
            certainty = 1
            text_hypo = Text.find_generate(trace_id=self.trace.id, content=self.description, certainty=certainty)
            self.output_hypos.append(text_hypo)
        return self.output_hypos

    def get_word_phrase(self, start_word):
        """
        Retrieves the phrase corresponding to a start word by performing a depth-first search on the dependency triples.

        :param start_word: The word to start the search from within the dependency triples.
        :type start_word: str
        :return: A list of words that form a phrase in the dependency tree starting from the given word.
        :rtype: list[str]
        """
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
        Applies fix 3 based on the provided fix line. 
        This fix involves inserting a word ("whom") between the noun and the relative clause to let the CoreNLP to infer the correct structure.

        :param fix_line: The line of information obtained from Prolog processing that indicates where the fix should be applied.
        :type fix_line: str
        :return: True if the fix was applied, False otherwise indicating no further fixes are needed.
        :rtype: bool
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
        # insert before the first word in the phrase
        insert_pos = int(phrase[0].rsplit("-", 1)[1]) - 1
        sents = self.description.split()[:]
        print(f"Fix 3: {triple_elements[0]}| insert {word_to_insert}| before: {sents[insert_pos]}| into sentence: {self.description}\n")      
        sents.insert(insert_pos, word_to_insert)
        sentence = " ".join(sents)
        self.description = sentence
        return True


class DepTreeFix5(DepTreeFix):
    fix_num = 5

    def __init__(self, hypothesis_ids, ks_ar, trace):
        super().__init__(trace, hypothesis_ids, ks_ar)
        
        self.word_hypos = dict()
    
    def set_input(self, dep_hypos):
        """
        Extends the parent class's set_input method by adding a set_words step for DepTreeFix5 operation.

        :param dep_hypos: A list of Dep objects representing the dependency hypotheses to be processed.
        :type dep_hypos: list[Dep]
        """
        super().set_input(dep_hypos) 
        self.set_words()

    def set_words(self): 
        """
        Set Word hypos based on Dep hypos
        """
        for dep in self.dep_hypos:
            subject_word = dep.subject_word
            object_word = dep.object_word

            self.word_hypos[subject_word.content_label] = subject_word
            self.word_hypos[object_word.content_label] = object_word
    
    def get_outputs(self):
        """
        Retrieves the processed dependency hypotheses after applying fix 5.

        :return: A list of output hypotheses generated after processing the dependency fixes.
        :rtype: list[Hypothesis]
        """
        if self.fix_applied:
            for triple in self.dep_triples:
                dep = triple[1]
                subject_label = triple[0][0].rsplit("-", 1)
                object_label = triple[2][0].rsplit("-", 1)

                subject = self.word_hypos[subject_label]
                object = self.word_hypos[object_label]

                certainty = 1
                dep = Dep.find_generate(dep=dep, subject_id=subject.id, object_id=object.id, trace_id=self.trace.id, certainty=certainty)
                dep.from_ks_ars = self.ks_ar.id
                self.output_hypos.append(dep)
        
        return self.output_hypos

    def fix(self, fix_line):
        """
        Applies fix 5 based on the provided fix line. 
        Fix 5 corrects specific dependency tree patterns involving incorrect 'obl' relations from verbs to nouns modified by VBG verbs.
        Utilizes 'cc_words' to discern genuine noun phrases from misparsed structures. If the VBG-NN pair is not a known noun phrase, 
        it rearranges the tree, reassigning the VBG as the object of the main verb.

        :param fix_line: The line of string obtained from Prolog processing that indicates the dependency relations involved in the 
                         problematic pattern.
        :type fix_line: str
        :return: True if the fix was applied, False otherwise indicating no further fixes are needed.
        :rtype: bool
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
