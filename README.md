# eval-exp
AST and Parser for simple expressions

### Schema for operation of the json

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

