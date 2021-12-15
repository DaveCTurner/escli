    escli - Interact with Elasticsearch from the shell

    Usage: escli ([--deployment DEPLOYMENT-ID] | [--cloud-region REGION]
                 [--cluster-id CLUSTER] | [--server ADDR] ([--username USERNAME]
                 [--password PASSWORD] | [--apikey ENVVAR])) [--hide-timing]
                 [--hide-headings] [--hide-status] [--hide-curl-equivalent]
                 [--show-curl-password] [--hide-deprecation-warnings] [--save]
                 ([--no-max-response-lines] | [--max-response-lines LINES])
                 [--log-file FILE] ([--certificate-store FILE] |
                 [--insecurely-bypass-certificate-verification])
      Interact with Elasticsearch from the shell

    Available options:
      --deployment DEPLOYMENT-ID
                               Cloud deployment ID
      --cloud-region REGION    Cloud region name
      --cluster-id CLUSTER     Cloud cluster ID
      --server ADDR            Base HTTP URI of the Elasticsearch server
      --username USERNAME      Elasticsearch username, for security-enabled clusters
      --password PASSWORD      Elasticsearch password, for security-enabled clusters
      --apikey ENVVAR          Environment variable holding API key
      --hide-timing            Hide timing information
      --hide-headings          Hide request/response headings
      --hide-status            Hide HTTP status code
      --hide-curl-equivalent   Hide `curl`-equivalent command
      --show-curl-password     Show password in `curl`-equivalent command
      --hide-deprecation-warnings
                               Hide deprecation warnings
      --save                   Save connection config to 'escli_config.json' for
                               future invocations
      --no-max-response-lines  Show an unlimited number of lines of response
      --max-response-lines LINES
                               Maximum number of lines of response to
                               show (default: 40)
      --log-file FILE          File in which to record output
      --certificate-store FILE Location of certificate store
      --insecurely-bypass-certificate-verification
                               Do not perform certificate verification
      -h,--help                Show this help text

Usage example:

    $ cat commands.txt 
    GET /

    # Comments are allowed
    PUT /i
    {"settings":{"number_of_replicas":1,"number_of_shards":1
    , "refresh_interval": -1
    }}
    # (so is whitespace in your JSON)

    # text/plain output is also supported
    GET /_cat/shards

    # (even in bulk requests)
    POST /i/_doc/_bulk
    {"index":{}}
    {}
    {"index":{}}
    {"content":{
      "foo":"bar"
    }}
    $ escli < commands.txt 
    # Server URI: http://localhost:9200

    # ========================================
    # Request: 
    GET /
    # at 2018-04-16T16:38:53.894Z
    # curl http://localhost:9200/

    # ----------------------------------------
    # Response: 
    # 200 OK
    # {
    #   "cluster_name": "elasticsearch",
    #   "tagline": "You Know, for Search",
    #   "name": "k_F7Bns",
    #   "version": {
    #     "build_snapshot": false,
    #     "build_hash": "10b1edd",
    #     "minimum_index_compatibility_version": "5.0.0",
    #     "minimum_wire_compatibility_version": "5.6.0",
    #     "build_date": "2018-02-16T19:01:30.685723Z",
    #     "number": "6.2.2",
    #     "lucene_version": "7.2.1"
    #   },
    #   "cluster_uuid": "09rWMOyiQRqDD3MS5BZalw"
    # }
    # at 2018-04-16T16:38:53.916Z
    # (0.022017s elapsed)

    # ========================================
    # Request: 
    PUT /i
    {
      "settings": {
        "number_of_shards": 1,
        "number_of_replicas": 1,
        "refresh_interval": -1
      }
    }
    # at 2018-04-16T16:38:53.919Z
    # curl -XPUT http://localhost:9200/i -H 'Content-type: application/json' --data-binary $'{"settings":{"number_of_shards":1,"number_of_replicas":1,"refresh_interval":-1}}'

    # ----------------------------------------
    # Response: 
    # 200 OK
    # {
    #   "shards_acknowledged": true,
    #   "acknowledged": true,
    #   "index": "i"
    # }
    # at 2018-04-16T16:38:54.052Z
    # (0.133428s elapsed)

    # ========================================
    # Request: 
    GET /_cat/shards
    # at 2018-04-16T16:38:54.053Z
    # curl http://localhost:9200/_cat/shards

    # ----------------------------------------
    # Response: 
    # 200 OK
    # i 0 p STARTED    0 230b 127.0.0.1 k_F7Bns
    # i 0 r UNASSIGNED                  
    # 
    # at 2018-04-16T16:38:54.095Z
    # (0.042391s elapsed)

    # ========================================
    # Request: 
    POST /i/_doc/_bulk
    {"index":{}}
    {}
    {"index":{}}
    {"content":{"foo":"bar"}}
    # at 2018-04-16T16:38:54.095Z
    # curl http://localhost:9200/i/_doc/_bulk -H 'Content-type: application/x-ndjson' --data-binary $'{"index":{}}\n{}\n{"index":{}}\n{"content":{"foo":"bar"}}\n'

    # ----------------------------------------
    # Response: 
    # 200 OK
    # {
    #   "took": 204,
    #   "items": [
    #     {
    #       "index": {
    #         "status": 201,
    #         "_type": "_doc",
    #         "_primary_term": 1,
    #         "_id": "i75Sz2IBH3lRQ0iQ0Z6b",
    #         "_shards": {
    #           "successful": 1,
    #           "total": 2,
    #           "failed": 0
    #         },
    #         "_index": "i",
    #         "result": "created",
    #         "_version": 1,
    #         "_seq_no": 0
    #       }
    #     },
    #     {
    #       "index": {
    #         "status": 201,
    #         "_type": "_doc",
    #         "_primary_term": 1,
    #         "_id": "jL5Sz2IBH3lRQ0iQ0Z6c",
    #         "_shards": {
    #           "successful": 1,
    #           "total": 2,
    #           "failed": 0
    #         },
    #         "_index": "i",
    #         "result": "created",
    #         "_version": 1,
    #         "_seq_no": 1
    #       }
    #     }
    #   ],
    #   "errors": false
    # }
    # at 2018-04-16T16:38:54.308Z
    # (0.212651s elapsed)


