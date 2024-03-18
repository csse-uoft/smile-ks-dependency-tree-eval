# smile_ks_qa0
SMILE Knowledge Source: QA0




# Setup

#### 1 GraphDB Installation
- [GraphDB](https://www.ontotext.com/products/graphdb/) can be installed for your distribution.
- Make sure it's running port `7200`, e.g. [http://localhost:7200](http://localhost:7200).
- Make sure you have GraphDB running on [http://localhost:7200](http://localhost:7200).

#### 2 GraphDB Repository Creation
- For testing, make sure the username and password are set to `admin`
- Create a new test repository. Go to [http://localhost:7200](http://localhost:7200)
  - Create new repository:
    - Name the repository (Repository ID) as `smile`
    - Set context index to True *(checked).
    - Set query timeout to 45 second.
    - Set the `Throw exception on query timeout` checkmark to True (checked)
    - Click on create repository.
  - Make sure the repository rin in "Running" state.
- [See instruction below for troubleshooting](#user-content-graphdb-and-docker-configuration)


#### 3 GraphDB Configuration
A few notes on configurting SMILE to connect to the database.
- The main SMILE Flask application can be configured in the [config/local_config.yml](config/local_config.yml) file.
- Knowledge source that are running in a Docker instance must use the "Docker" version of the config file: [config/local_config_test.yml](config/local_config_test.yml).



#### 4 Setup smile-ks-dependency-tree-eval
`conda env create -f PyDepTrees.yml`


# Running KnowldgeSource
## Run NLP Server
You will need two terminals. In teh first termina,, run teh server, as shown below. Run teh knwodlge source in another terminal.

Term 1, run:
`cd src/smile_ks_dependency_tree_eval/libs/`
`./corenlp/nlp_server.sh`
This will start a NLP server at [http://localhost:9000](http://localhost:9000). It will run in the background.


## To run example
`conda activate PyDepTrees`
`cd src`
`python -m smile_ks_qa0.main`


## To run KnowledgeSource Listener
`conda activate PyDepTrees`
`cd src`
`python -m smile_ks_dependency_tree_eval.listener`

## To run KnowledgeSource Console
`conda activate PyDepTrees`
`cd src`
`python -m smile_ks_dependency_tree_eval.console`


## To run KnowledgeSource Tests
`conda activate PyDepTrees`
`python -m tests.fix_tests`
