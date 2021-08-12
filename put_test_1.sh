#!/bin/bash

curl -X 'POST' \
  'http://localhost:8000/api/run' \
  -H 'accept: application/json;charset=utf-8' \
  -H 'Content-Type: application/json;charset=utf-8' \
  -d '{
  "machine": {
    "seed": 0,
    "resourceTagColor": {
    },
    "graph": {
      "stateEdges": {
        "additionalProp1": {
          "stateFormula": {
            "contents": "string",
            "tag": "SFAdd"
          },
          "to": 9223372036854776000,
          "resourceFilter": "string",
          "active": true,
          "from": 9223372036854776000
        },
        "additionalProp2": {
          "stateFormula": {
            "contents": "string",
            "tag": "SFAdd"
          },
          "to": 9223372036854776000,
          "resourceFilter": "string",
          "active": true,
          "from": 9223372036854776000
        },
        "additionalProp3": {
          "stateFormula": {
            "contents": "string",
            "tag": "SFAdd"
          },
          "to": 9223372036854776000,
          "resourceFilter": "string",
          "active": true,
          "from": 9223372036854776000
        }
      },
      "vertices": {
        "additionalProp1": {
          "nodeColor": "string",
          "nodeLabel": "string",
          "nodeTy": {
            "resourceTypes": [
              "string"
            ],
            "activation": "Passive",
            "tag": "Source"
          }
        },
        "additionalProp2": {
          "nodeColor": "string",
          "nodeLabel": "string",
          "nodeTy": {
            "resourceTypes": [
              "string"
            ],
            "activation": "Passive",
            "tag": "Source"
          }
        },
        "additionalProp3": {
          "nodeColor": "string",
          "nodeLabel": "string",
          "nodeTy": {
            "resourceTypes": [
              "string"
            ],
            "activation": "Passive",
            "tag": "Source"
          }
        }
      },
      "resourceEdges": {
        "additionalProp1": {
          "to": 9223372036854776000,
          "resourceFilter": "string",
          "interval": {
            "intervalFormula": {
              "tag": "RFAll"
            },
            "intervalCounter": 9223372036854776000
          },
          "resourceFormula": {
            "tag": "RFAll"
          },
          "from": 9223372036854776000,
          "limits": {
            "lower": 9223372036854776000,
            "upper": 9223372036854776000
          },
          "shuffleOrigin": true,
          "transfer": "IntervalTransfer"
        },
        "additionalProp2": {
          "to": 9223372036854776000,
          "resourceFilter": "string",
          "interval": {
            "intervalFormula": {
              "tag": "RFAll"
            },
            "intervalCounter": 9223372036854776000
          },
          "resourceFormula": {
            "tag": "RFAll"
          },
          "from": 9223372036854776000,
          "limits": {
            "lower": 9223372036854776000,
            "upper": 9223372036854776000
          },
          "shuffleOrigin": true,
          "transfer": "IntervalTransfer"
        },
        "additionalProp3": {
          "to": 9223372036854776000,
          "resourceFilter": "string",
          "interval": {
            "intervalFormula": {
              "tag": "RFAll"
            },
            "intervalCounter": 9223372036854776000
          },
          "resourceFormula": {
            "tag": "RFAll"
          },
          "from": 9223372036854776000,
          "limits": {
            "lower": 9223372036854776000,
            "upper": 9223372036854776000
          },
          "shuffleOrigin": true,
          "transfer": "IntervalTransfer"
        }
      }
    },
    "stateEdgeModifiers": {
      "disableNode": [
        9223372036854776000
      ],
      "modifyResourceFormula": {
        "additionalProp1": [
          {
            "contents": "string",
            "tag": "SFAdd"
          },
          {
            "contents": "string",
            "tag": "SFSub"
          },
          {
            "contents": "string",
            "tag": "SFMul"
          },
          {
            "contents": "string",
            "tag": "SFDiv"
          },
          {
            "contents": [
              "string",
              "string"
            ],
            "tag": "SFCondition"
          },
          {
            "contents": "string",
            "tag": "SFOverwrite"
          },
          {
            "contents": "string",
            "tag": "SFPercentage"
          },
          {
            "contents": [
              "string",
              "string"
            ],
            "tag": "SFRange"
          },
          {
            "contents": "string",
            "tag": "SFInterval"
          },
          {
            "contents": 9223372036854776000,
            "tag": "SFConstant"
          },
          {
            "tag": "SFTrigger"
          },
          {
            "tag": "SFReverseTrigger"
          },
          {
            "contents": "string",
            "tag": "SFVariable"
          }
        ],
        "additionalProp2": [
          {
            "contents": "string",
            "tag": "SFAdd"
          },
          {
            "contents": "string",
            "tag": "SFSub"
          },
          {
            "contents": "string",
            "tag": "SFMul"
          },
          {
            "contents": "string",
            "tag": "SFDiv"
          },
          {
            "contents": [
              "string",
              "string"
            ],
            "tag": "SFCondition"
          },
          {
            "contents": "string",
            "tag": "SFOverwrite"
          },
          {
            "contents": "string",
            "tag": "SFPercentage"
          },
          {
            "contents": [
              "string",
              "string"
            ],
            "tag": "SFRange"
          },
          {
            "contents": "string",
            "tag": "SFInterval"
          },
          {
            "contents": 9223372036854776000,
            "tag": "SFConstant"
          },
          {
            "tag": "SFTrigger"
          },
          {
            "tag": "SFReverseTrigger"
          },
          {
            "contents": "string",
            "tag": "SFVariable"
          }
        ],
        "additionalProp3": [
          {
            "contents": "string",
            "tag": "SFAdd"
          },
          {
            "contents": "string",
            "tag": "SFSub"
          },
          {
            "contents": "string",
            "tag": "SFMul"
          },
          {
            "contents": "string",
            "tag": "SFDiv"
          },
          {
            "contents": [
              "string",
              "string"
            ],
            "tag": "SFCondition"
          },
          {
            "contents": "string",
            "tag": "SFOverwrite"
          },
          {
            "contents": "string",
            "tag": "SFPercentage"
          },
          {
            "contents": [
              "string",
              "string"
            ],
            "tag": "SFRange"
          },
          {
            "contents": "string",
            "tag": "SFInterval"
          },
          {
            "contents": 9223372036854776000,
            "tag": "SFConstant"
          },
          {
            "tag": "SFTrigger"
          },
          {
            "tag": "SFReverseTrigger"
          },
          {
            "contents": "string",
            "tag": "SFVariable"
          }
        ]
      },
      "enableNode": [
        9223372036854776000
      ],
      "enableResourceEdge": [
        9223372036854776000
      ],
      "triggerNode": [
        9223372036854776000
      ],
      "modifyNode": {
        "additionalProp1": [
          {
            "contents": "string",
            "tag": "SFAdd"
          },
          {
            "contents": "string",
            "tag": "SFSub"
          },
          {
            "contents": "string",
            "tag": "SFMul"
          },
          {
            "contents": "string",
            "tag": "SFDiv"
          },
          {
            "contents": [
              "string",
              "string"
            ],
            "tag": "SFCondition"
          },
          {
            "contents": "string",
            "tag": "SFOverwrite"
          },
          {
            "contents": "string",
            "tag": "SFPercentage"
          },
          {
            "contents": [
              "string",
              "string"
            ],
            "tag": "SFRange"
          },
          {
            "contents": "string",
            "tag": "SFInterval"
          },
          {
            "contents": 9223372036854776000,
            "tag": "SFConstant"
          },
          {
            "tag": "SFTrigger"
          },
          {
            "tag": "SFReverseTrigger"
          },
          {
            "contents": "string",
            "tag": "SFVariable"
          }
        ],
        "additionalProp2": [
          {
            "contents": "string",
            "tag": "SFAdd"
          },
          {
            "contents": "string",
            "tag": "SFSub"
          },
          {
            "contents": "string",
            "tag": "SFMul"
          },
          {
            "contents": "string",
            "tag": "SFDiv"
          },
          {
            "contents": [
              "string",
              "string"
            ],
            "tag": "SFCondition"
          },
          {
            "contents": "string",
            "tag": "SFOverwrite"
          },
          {
            "contents": "string",
            "tag": "SFPercentage"
          },
          {
            "contents": [
              "string",
              "string"
            ],
            "tag": "SFRange"
          },
          {
            "contents": "string",
            "tag": "SFInterval"
          },
          {
            "contents": 9223372036854776000,
            "tag": "SFConstant"
          },
          {
            "tag": "SFTrigger"
          },
          {
            "tag": "SFReverseTrigger"
          },
          {
            "contents": "string",
            "tag": "SFVariable"
          }
        ],
        "additionalProp3": [
          {
            "contents": "string",
            "tag": "SFAdd"
          },
          {
            "contents": "string",
            "tag": "SFSub"
          },
          {
            "contents": "string",
            "tag": "SFMul"
          },
          {
            "contents": "string",
            "tag": "SFDiv"
          },
          {
            "contents": [
              "string",
              "string"
            ],
            "tag": "SFCondition"
          },
          {
            "contents": "string",
            "tag": "SFOverwrite"
          },
          {
            "contents": "string",
            "tag": "SFPercentage"
          },
          {
            "contents": [
              "string",
              "string"
            ],
            "tag": "SFRange"
          },
          {
            "contents": "string",
            "tag": "SFInterval"
          },
          {
            "contents": 9223372036854776000,
            "tag": "SFConstant"
          },
          {
            "tag": "SFTrigger"
          },
          {
            "tag": "SFReverseTrigger"
          },
          {
            "contents": "string",
            "tag": "SFVariable"
          }
        ]
      },
      "disableResourceEdge": [
        9223372036854776000
      ]
    },
    "time": 9223372036854776000
  },
  "activeNodes": [
    9223372036854776000
  ]
}'
