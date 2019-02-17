
# eval-exp

[![Build Status](https://travis-ci.org/saswata-dutta/eval-exp.svg?branch=master)](https://travis-ci.org/saswata-dutta/eval-exp)

[![Coverage Status](https://coveralls.io/repos/github/saswata-dutta/eval-exp/badge.svg?branch=develop)](https://coveralls.io/github/saswata-dutta/eval-exp?branch=develop)

AST and Json-Parser for simple expressions

### Json Schema for defining expressions

```
  {
    "type": <String representing the type of the entity>,
    "key": <String representing the key of the symbol>,
    "value": <String or Number representing the literal>,
    "cond": <Object representing the condition for if statement>,
    "lhs": <Object representing the lhs operand>,
    "rhs": <Object representing the rhs operand>
  }
```
  
  ### Build
  
  - Execute `sbt assembly` to run tests and build the uber jar. 
  - Execute `sbt clean coverage test coverageReport` to run tests and generate the coverage report.
