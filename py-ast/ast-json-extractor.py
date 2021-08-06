import json
import os
from ast import parse
from ast2json import str2json

# path environment `PYANALYZER_HOME` must be set
DIR_NAME = "py-ast"
ROOT_PATH = os.environ["PYANALYZER_HOME"] + "/" + DIR_NAME
PY_NAME = "temp.py"
JSON_NAME = "temp.json"
PY_PATH = ROOT_PATH + "/" + PY_NAME
JSON_PATH = ROOT_PATH + "/" + JSON_NAME

def main():
    with open(PY_PATH, 'r') as rf:
        source = rf.read()
        ast_json = str2json(source)

    with open(JSON_PATH, 'w') as wf:
        wf.write(json.dumps(ast_json, indent=4)) 

if __name__ == '__main__':
    main()
            
