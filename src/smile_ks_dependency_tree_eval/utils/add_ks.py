import re, os, tqdm
from owlready2 import default_world, ObjectProperty, DataProperty, rdfs, Thing 
from py2graphdb.config import config as CONFIG
from py2graphdb.utils.db_utils import resolve_nm_for_dict, PropertyList

smile = default_world.get_ontology(CONFIG.NM)
with smile:
    from smile_base.Model.controller.ks import Ks

def add_ks():
    kss = Ks.search(props={smile.hasPyName:'DepTreeFix3'}, how='all')
    kss += Ks.search(props={smile.hasPyName:'DepTreeFix5'}, how='all')
    for ks in set(kss):
        ks.delete()

    ALL_KS_FORMATS = {
        'DepTreeFix3'   : ['DepTreeFix3', True, ["Dep"], ["Text"]],
        'DepTreeFix5'   : ['DepTreeFix5', True, ["Dep"], ["Dep"]]
    }

    for ks_name, fields in ALL_KS_FORMATS.items():
        Ks.ALL_KS_FORMATS[ks_name] = fields
    for ks_name in ALL_KS_FORMATS.keys():
        Ks.initialize_ks(ks_name)

