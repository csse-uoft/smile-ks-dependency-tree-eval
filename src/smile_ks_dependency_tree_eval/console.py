import re, os, tqdm
from src.smile_ks_dependency_tree_eval.listener import DepTreeFix, DepTreeFix3, DepTreeFix5, Text, Trace, Ks, KSAR, Hypothesis, Program, Dep
from src.smile_ks_dependency_tree_eval.utils import add_ks
from owlready2 import default_world, ObjectProperty, DataProperty, rdfs, Thing 
from py2graphdb.config import config as CONFIG
from py2graphdb.utils.db_utils import resolve_nm_for_dict, PropertyList
from py2graphdb.ontology.operators import *
from smile_base.utils import init_db

if not os.path.exists(CONFIG.LOG_DIR):
    os.makedirs(CONFIG.LOG_DIR)

smile = default_world.get_ontology(CONFIG.NM)
