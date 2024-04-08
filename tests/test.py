
import unittest
from src.smile_ks_dependency_tree_eval.listener import *

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

def gen_list_deps(triple: list):

    list_of_dep = []
    for lists in triple: 
        MockPos_Sub = MockPos(tag = lists[0][1])
        MockPos_Obj = MockPos(tag = lists[2][1])
        MockWord_Sub = MockWord(content=lists[0][0].split('-')[0], label=lists[0][0], pos=MockPos_Sub)
        MockWord_Obj = MockWord(content=lists[2][0].split('-')[0], label=lists[2][0], pos=MockPos_Obj)
        MockDep = DepMock(dep_label = lists[1], subject_word = MockWord_Sub, object_word = MockWord_Obj)
        list_of_dep.append(MockDep)
    return list_of_dep



class Test_Fix_3(unittest.TestCase):
    def test_setinput3_need_fix(self):
        """
        Test fix 3
        """
        sent_3 = "Mary hugged the boy the girl left."
        sent_3_fixed = 'Mary hugged the boy whom the girl left .'
        triples_3 = [[['ROOT-0', 'NONE'], 'ROOT', ['hugged-2', 'VBD']], [['hugged-2', 'VBD'], 'nsubj', ['Mary-1', 'NNP']], [['boy-4', 'NN'], 'det', ['the-3', 'DT']], [['hugged-2', 'VBD'], 'obj', ['boy-4', 'NN']], [['girl-6', 'NN'], 'det', ['the-5', 'DT']], [['left-7', 'VBD'], 'nsubj', ['girl-6', 'NN']], [['hugged-2', 'VBD'], 'parataxis', ['left-7', 'VBD']], [['hugged-2', 'VBD'], 'punct', ['.-8', '.']]]
        triple_3_fixed = [[['ROOT-0', 'NONE'], 'ROOT', ['hugged-2', 'VBD']], [['hugged-2', 'VBD'], 'nsubj', ['Mary-1', 'NNP']], [['boy-4', 'NN'], 'det', ['the-3', 'DT']], [['hugged-2', 'VBD'], 'obj', ['boy-4', 'NN']], [['left-8', 'VBD'], 'obj', ['whom-5', 'WP']], [['girl-7', 'NN'], 'det', ['the-6', 'DT']], [['left-8', 'VBD'], 'nsubj', ['girl-7', 'NN']], [['boy-4', 'NN'], 'acl:relcl', ['left-8', 'VBD']], [['hugged-2', 'VBD'], 'punct', ['.-9', '.']]]

        list_of_dep = gen_list_deps(triples_3)
        result = dep_tree3.set_input(list_of_dep)
        self.assertEqual(True, dep_tree3.fix_applied)
    
    def test_setinput3_nofix_needed(self):
        """
        Test fix 3
        """
        sent_3 = "Mary hugged the boy the girl left."
        sent_3_fixed = 'Mary hugged the boy whom the girl left .'
        triples_3 = [[['ROOT-0', 'NONE'], 'ROOT', ['hugged-2', 'VBD']], [['hugged-2', 'VBD'], 'nsubj', ['Mary-1', 'NNP']], [['boy-4', 'NN'], 'det', ['the-3', 'DT']], [['hugged-2', 'VBD'], 'obj', ['boy-4', 'NN']], [['girl-6', 'NN'], 'det', ['the-5', 'DT']], [['left-7', 'VBD'], 'nsubj', ['girl-6', 'NN']], [['hugged-2', 'VBD'], 'parataxis', ['left-7', 'VBD']], [['hugged-2', 'VBD'], 'punct', ['.-8', '.']]]
        triple_3_fixed = [[['ROOT-0', 'NONE'], 'ROOT', ['hugged-2', 'VBD']], [['hugged-2', 'VBD'], 'nsubj', ['Mary-1', 'NNP']], [['boy-4', 'NN'], 'det', ['the-3', 'DT']], [['hugged-2', 'VBD'], 'obj', ['boy-4', 'NN']], [['left-8', 'VBD'], 'obj', ['whom-5', 'WP']], [['girl-7', 'NN'], 'det', ['the-6', 'DT']], [['left-8', 'VBD'], 'nsubj', ['girl-7', 'NN']], [['boy-4', 'NN'], 'acl:relcl', ['left-8', 'VBD']], [['hugged-2', 'VBD'], 'punct', ['.-9', '.']]]

        list_of_dep = gen_list_deps(triple_3_fixed)
        result = dep_tree3.set_input(list_of_dep)
        self.assertEqual(False, dep_tree3.fix_applied) 

class Test_Fix_5(unittest.TestCase):
    def test_setinput5_need_fix(self):
        """
        Test Fix 5
        """
        sent_5 = 'The school insisted on providing documents.'
        sent_5_fixed = 'The school insisted on providing documents .'
        triple_5 = [[['ROOT-0', 'NONE'], 'ROOT', ['insisted-3', 'VBD']], [['school-2', 'NN'], 'det', ['The-1', 'DT']], [['insisted-3', 'VBD'], 'nsubj', ['school-2', 'NN']], [['documents-6', 'NNS'], 'case', ['on-4', 'IN']], [['documents-6', 'NNS'], 'amod', ['providing-5', 'VBG']], [['insisted-3', 'VBD'], 'obl', ['documents-6', 'NNS']], [['insisted-3', 'VBD'], 'punct', ['.-7', '.']]]
        triple_5_fixed = [[['ROOT-0', 'NONE'], 'ROOT', ['insisted-3', 'VBD']], [['school-2', 'NN'], 'det', ['The-1', 'DT']], [['insisted-3', 'VBD'], 'nsubj', ['school-2', 'NN']], [['documents-6', 'NNS'], 'case', ['on-4', 'IN']], [['providing-5', 'VBG'], 'obj', ['documents-6', 'NNS']], [['insisted-3', 'VBD'], 'obl', ['providing-5', 'VBG']], [['insisted-3', 'VBD'], 'punct', ['.-7', '.']]]

        list_of_dep = gen_list_deps(triple_5)
        result = dep_tree5.set_input(list_of_dep)
        self.assertEqual(True, dep_tree5.fix_applied)

    def test_setinput5_nofix_needed(self):
        """
        Test Fix 5
        """
        sent_5 = 'The school insisted on providing documents.'
        sent_5_fixed = 'The school insisted on providing documents .'
        triple_5 = [[['ROOT-0', 'NONE'], 'ROOT', ['insisted-3', 'VBD']], [['school-2', 'NN'], 'det', ['The-1', 'DT']], [['insisted-3', 'VBD'], 'nsubj', ['school-2', 'NN']], [['documents-6', 'NNS'], 'case', ['on-4', 'IN']], [['documents-6', 'NNS'], 'amod', ['providing-5', 'VBG']], [['insisted-3', 'VBD'], 'obl', ['documents-6', 'NNS']], [['insisted-3', 'VBD'], 'punct', ['.-7', '.']]]
        triple_5_fixed = [[['ROOT-0', 'NONE'], 'ROOT', ['insisted-3', 'VBD']], [['school-2', 'NN'], 'det', ['The-1', 'DT']], [['insisted-3', 'VBD'], 'nsubj', ['school-2', 'NN']], [['documents-6', 'NNS'], 'case', ['on-4', 'IN']], [['providing-5', 'VBG'], 'obj', ['documents-6', 'NNS']], [['insisted-3', 'VBD'], 'obl', ['providing-5', 'VBG']], [['insisted-3', 'VBD'], 'punct', ['.-7', '.']]]

        list_of_dep = gen_list_deps(triple_5_fixed)
        result = dep_tree5.set_input(list_of_dep)
        self.assertEqual(False, dep_tree5.fix_applied)
    

if __name__ == '__main__':
    unittest.main()