import unittest
# from src.smile_ks_dependency_tree_eval.listener import *

class Dummy():
    id = None
    def __init__(self, id):
        self.id = id

ks_ar = Dummy(5)
trace = Dummy(10)
hypo_ids = []
#dep_tree3 = DepTreeFix3(hypothesis_ids=hypo_ids, ks_ar=ks_ar, trace=trace)
#dep_tree5 = DepTreeFix5(hypothesis_ids=hypo_ids, ks_ar=ks_ar, trace=trace)

class DepMock:
    def __init__(self, dep_label, subject_word, object_word):
        self.dep = dep_label   ### 'ROOT'
        self.subject_word = subject_word ## word id
        self.object_word = object_word    ## word id
    
    def __str__(self) -> str:
        return f"({MockWord.get(self.subject_word)}, {self.dep}, {MockWord.get(self.subject_word)})"

class MockWord:
    instances = {}
    curr_id = 0

    def __init__(self, content=None, label=None, pos=None):
        self.id = MockWord.curr_id
        MockWord.curr_id += 1
        self.content = content ## 'hugged'
        self.content_label = label  ## 'hugged-2'
        self.pos = pos      ## pos id
        MockWord.instances[id] = self

    @classmethod
    def get(cls, id):
        return cls.instances.get(id)

    def __str__(self) -> str:
        return f"({self.content_label}, {MockPos.get(self.pos)})"


class MockPos:
    instances = {}
    curr_id = 0

    def __init__(self, tag=None):
        self.id = MockPos.curr_id
        MockPos.curr_id += 1
        self.tag = tag ## 'VBD'
        MockPos.instances[id] = self
    
    @classmethod
    def get(cls, id):
        return cls.instances.get(id)
    
    def __str__(self) -> str:
        return f"{self.tag}"


def gen_list_deps(triples: list):

    list_of_dep = []
    for triple in triples: 
        MockPos_Sub = MockPos(tag = triple[0][1])
        MockPos_Obj = MockPos(tag = triple[2][1])
        MockWord_Sub = MockWord(content=triple[0][0].rsplit('-', 1)[0], label=triple[0][0], pos=MockPos_Sub.id)
        MockWord_Obj = MockWord(content=triple[2][0].rsplit('-', 1)[0], label=triple[2][0], pos=MockPos_Obj.id)
        MockDep = DepMock(dep_label = triple[1], subject_word = MockWord_Sub.id, object_word = MockWord_Obj.id)
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

        triples_3 = [[['hugged-2', 'VBD'], 'punct', ['.-8', '.']], [['boy-4', 'NN'], 'det', ['the-3', 'DT']], [['hugged-2', 'VBD'], 'obj', ['boy-4', 'NN']], [['left-7', 'VBD'], 'nsubj', ['girl-6', 'NN']], [['girl-6', 'NN'], 'det', ['the-5', 'DT']], [['hugged-2', 'VBD'], 'nsubj', ['Mary-1', 'NNP']], [['boy-4', 'NN'], 'acl:relcl', ['left-7', 'VBD']]]
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
    # unittest.main()
    triple_5 = [[['ROOT-0', 'NONE'], 'ROOT', ['insisted-3', 'VBD']], [['school-2', 'NN'], 'det', ['The-1', 'DT']], [['insisted-3', 'VBD'], 'nsubj', ['school-2', 'NN']], [['documents-6', 'NNS'], 'case', ['on-4', 'IN']], [['documents-6', 'NNS'], 'amod', ['providing-5', 'VBG']], [['insisted-3', 'VBD'], 'obl', ['documents-6', 'NNS']], [['insisted-3', 'VBD'], 'punct', ['.-7', '.']]]
    deps = gen_list_deps(triple_5)
    for dep in deps:
        print(dep)
