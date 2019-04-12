# eval-exp 
[![Build Status](https://travis-ci.org/saswata-dutta/eval-exp.svg?branch=master)](https://travis-ci.org/saswata-dutta/eval-exp)
[![](https://jitpack.io/v/saswata-dutta/eval-exp.svg)](https://jitpack.io/#saswata-dutta/eval-exp)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/de5c9be4524641488e45da1a74b3d09a)](https://app.codacy.com/app/sasdutta/eval-exp?utm_source=github.com&utm_medium=referral&utm_content=saswata-dutta/eval-exp&utm_campaign=Badge_Grade_Dashboard)
[![CodeFactor](https://www.codefactor.io/repository/github/saswata-dutta/eval-exp/badge)](https://www.codefactor.io/repository/github/saswata-dutta/eval-exp)
[![codecov](https://codecov.io/gh/saswata-dutta/eval-exp/branch/master/graph/badge.svg)](https://codecov.io/gh/saswata-dutta/eval-exp)
[![Coverage Status](https://coveralls.io/repos/github/saswata-dutta/eval-exp/badge.svg?branch=develop)](https://coveralls.io/github/saswata-dutta/eval-exp?branch=develop)
[![Dependencies](https://app.updateimpact.com/badge/1111677947179307008/eval-exp.svg?config=compile)](https://app.updateimpact.com/latest/1111677947179307008/eval-exp)
[![Known Vulnerabilities](https://snyk.io/test/github/saswata-dutta/eval-exp/badge.svg?targetFile=build.sbt)](https://snyk.io/test/github/saswata-dutta/eval-exp?targetFile=build.sbt)

AST and Json-Parser for simple expressions

## Json Schema for defining expressions

```javascript
  {
    "type": <String representing the type of the entity>,
    "key": <String representing the key of the symbol>,
    "value": <String or Number representing the literal>,
    "cond": <Object representing the condition for IF statement>,
    "lhs": <Object representing the lhs operand>,
    "rhs": <Object representing the rhs operand>
  }
```
  
## Build

- Execute `sbt assembly` to run tests and build the uber jar.
- Execute `sbt clean coverage test coverageReport` to run tests and generate the coverage report.

## Json Generator Util

Use the [javascript snippet](https://gist.github.com/saswata-dutta/aeaf105908d83eea185f312c5c2cb711) to easily generate the json for complex expressions.
