#!/bin/bash

curl -X 'POST' \
  'http://localhost:8000/api/run' \
  -v \
  -H 'accept: application/json;charset=utf-8' \
  -H 'Content-Type: application/json;charset=utf-8' \
  -d '{
  "machine":
    {
        "seed": 0,
        "resourceTagColor": {},
        "graph": {
            "stateEdges": {},
            "vertices": {
                "9": {
                    "nodeColor": "black",
                    "nodeLabel": "",
                    "nodeTy": {
                        "activation": "Passive",
                        "overflow": "OverflowBlock",
                        "resources": [],
                        "pushPullAction": {
                            "contents": "PullAny",
                            "tag": "Pulling"
                        },
                        "tag": "Pool"
                    }
                },
                "7": {
                    "nodeColor": "black",
                    "nodeLabel": "",
                    "nodeTy": {
                        "waitingResources": [],
                        "activation": "Passive",
                        "tag": "Delay"
                    }
                },
                "6": {
                    "nodeColor": "black",
                    "nodeLabel": "",
                    "nodeTy": {
                        "activation": "Passive",
                        "overflow": "OverflowBlock",
                        "resources": [
                            {
                                "uUID": "00b3d467-4550-4a74-b985-585e4781958b",
                                "tag": "Black"
                            },
                            {
                                "uUID": "1c5f964b-2ff8-4357-8ec3-e56f6cdd6982",
                                "tag": "Black"
                            },
                            {
                                "uUID": "5cfc21c3-1805-40bb-abfd-76dd153c790a",
                                "tag": "Black"
                            },
                            {
                                "uUID": "766a8702-96bb-4096-9fbc-5ec7a0fd3d08",
                                "tag": "Black"
                            },
                            {
                                "uUID": "76dfc7b7-5790-48c2-8aa6-6af9bc1b7373",
                                "tag": "Black"
                            },
                            {
                                "uUID": "79517836-9016-430f-891f-6e457e359637",
                                "tag": "Black"
                            },
                            {
                                "uUID": "80284b94-9fef-4acd-bb06-b60b455f817e",
                                "tag": "Black"
                            },
                            {
                                "uUID": "9f226946-c8dd-49f8-b1f7-f7f0bce1962b",
                                "tag": "Black"
                            },
                            {
                                "uUID": "d21bdb69-e34f-4556-aa7a-c0d5469801e1",
                                "tag": "Black"
                            },
                            {
                                "uUID": "f3624da6-2d8b-4f2c-82de-2a4d32ad0ef6",
                                "tag": "Black"
                            }
                        ],
                        "pushPullAction": {
                            "contents": "PullAny",
                            "tag": "Pulling"
                        },
                        "tag": "Pool"
                    }
                }
            },
            "resourceEdges": {
                "8": {
                    "to": 7,
                    "interval": {
                        "counter": 0,
                        "formula": {
                            "contents": 1,
                            "tag": "RFConstant"
                        }
                    },
                    "resourceFormula": {
                        "contents": 1,
                        "tag": "RFConstant"
                    },
                    "from": 6,
                    "limits": {},
                    "shuffleOrigin": false,
                    "transfer": "IntervalTransfer"
                },
                "10": {
                    "to": 9,
                    "interval": {
                        "counter": 0,
                        "formula": {
                            "contents": 1,
                            "tag": "RFConstant"
                        }
                    },
                    "resourceFormula": {
                        "contents": 3,
                        "tag": "RFConstant"
                    },
                    "from": 7,
                    "limits": {},
                    "shuffleOrigin": false,
                    "transfer": "IntervalTransfer"
                }
            }
        },
        "time": 0
    }
  ,
  "activeNodes": [
  ]
}'

