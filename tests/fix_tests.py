import re, os, tqdm
from owlready2 import default_world, onto_path, ObjectProperty, DataProperty, rdfs, Thing 
onto_path.append('./ontology_cache/')
from py2graphdb.config import config as CONFIG
smile = default_world.get_ontology(CONFIG.NM)
with smile:
    from py2graphdb.Models.graph_node import GraphNode, SPARQLDict
    from py2graphdb.utils.db_utils import resolve_nm_for_dict, PropertyList
    from pyscript.app.scripts import nlp_parser
    from smile_base.Model.knowledge_source.knowledge_source import KnowledgeSource

    from smile_base.Model.data_level.phrase import Phrase
    from smile_base.Model.data_level.text import Text
    from smile_base.Model.data_level.sentence import Sentence
    from smile_base.Model.data_level.hypothesis import Hypothesis
    from smile_base.Model.controller.ks import Ks
    from smile_base.Model.controller.ks_ar import KSAR
    from smile_base.Model.controller.trace import Trace

with smile:
    text = "Mary hugged the boy the girl left."
    
    # text = "The school insisted on providing documents."
    print('Depedency tree generation started')
    class Dummy():
        id = None
        def __init__(self, id):
            self.id = id
    ks_ar = Dummy(5)
    trace = Dummy(10)
    hypo_ids = []
    dep_tree3 = DepTreeFix3(hypothesis_ids=hypo_ids, ks_ar=ks_ar, trace=trace)
    dep_tree5 = DepTreeFix5(hypothesis_ids=hypo_ids, ks_ar=ks_ar, trace=trace)

    class DepMock:
        def __init__(self, dep_label, subject_word, object_word):
            self.dep = dep_label   ### 'ROOT'
            self.subject_word = subject_word  ## MockWord class object 
            self.object_word = object_word    ## MockWord class object 

    class MockWord:
        def __init__(self, content=None, label=None, pos=None):
            self.content = content ## 'hugged'
            self.label = label  ## 'hugged-2'
            self.pos = pos      ## MockPos object 

    class MockPos:
        def __init__(self, tag = None):
            self.tag = tag ## 'VBD'

    # triples = [[['ROOT-0', 'NONE'], 'ROOT', ['hugged-2', 'VBD']], 
    #            [['hugged-2', 'VBD'], 'nsubj', ['Mary-1', 'NNP']], 
    #            [['boy-4', 'NN'], 'det', ['the-3', 'DT']], 
    #            [['hugged-2', 'VBD'], 'obj', ['boy-4', 'NN']], [['girl-6', 'NN'], 'det', ['the-5', 'DT']], 
    #            [['left-7', 'VBD'], 'nsubj', ['girl-6', 'NN']], [['hugged-2', 'VBD'], 'parataxis', ['left-7', 'VBD']], 
    #            [['hugged-2', 'VBD'], 'punct', ['.-8', '.']]]
    
    triples_fix = [[['ROOT-0', 'NONE'], 'ROOT', ['hugged-2', 'VBD']], [['hugged-2', 'VBD'], 'nsubj', ['Mary-1', 'NNP']], [['boy-4', 'NN'], 'det', ['the-3', 'DT']], [['hugged-2', 'VBD'], 'obj', ['boy-4', 'NN']], [['girl-6', 'NN'], 'det', ['the-5', 'DT']], [['left-7', 'VBD'], 'nsubj', ['girl-6', 'NN']], [['hugged-2', 'VBD'], 'parataxis', ['left-7', 'VBD']], [['hugged-2', 'VBD'], 'punct', ['.-8', '.']]]
    # triples_fix = [[['ROOT-0', 'NONE'], 'ROOT', ['insisted-3', 'VBD']], [['school-2', 'NN'], 'det', ['The-1', 'DT']], [['insisted-3', 'VBD'], 'nsubj', ['school-2', 'NN']], [['documents-6', 'NNS'], 'case', ['on-4', 'IN']], [['documents-6', 'NNS'], 'amod', ['providing-5', 'VBG']], [['insisted-3', 'VBD'], 'obl', ['documents-6', 'NNS']], [['insisted-3', 'VBD'], 'punct', ['.-7', '.']]]


    list_of_dep = []
    for lists in triples_fix: 
        MockPos_Sub = MockPos(tag = lists[0][1])
        MockPos_Obj = MockPos(tag = lists[2][1])
        MockWord_Sub = MockWord(content=lists[0][0].split('-')[0], label=lists[0][0], pos=MockPos_Sub)
        MockWord_Obj = MockWord(content=lists[2][0].split('-')[0], label=lists[2][0], pos=MockPos_Obj)
        MockDep = DepMock(dep_label = lists[1], subject_word = MockWord_Sub, object_word = MockWord_Obj)
        list_of_dep.append(MockDep)

    print(f"result: {dep_tree3.set_input(list_of_dep)}")

    