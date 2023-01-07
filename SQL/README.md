### An implementaion of mini SQL

This is a homework for functional programming course.

Author: Michael Polyntsov, arno9148@gmail.com

Usage:
...


Implemented language:

SQL-99 syntax:
SELECT [ DISTINCT | ALL ]
{Column expression [ AS name ]} [ ,... ] | *
FROM <Table reference> [ {,<Table reference>} ... ]
[ WHERE search condition ]
[ GROUP BY Columns [ HAVING condition ] ]
[ORDER BY {col\_name | expr | position} [ASC | DESC],...]
[LIMIT {[offset,] row\_count | row\_count OFFSET offset}]
[PROCEDURE procedure\_name(argument\_list)]
[INTO OUTFILE 'file\_name' export\_options |
 INTO DUMPFILE 'file\_name' |
 INTO var\_name [, var\_name]]
[FOR UPDATE | LOCK IN SHARE MODE]

We support only a subset of it (the mini SQL):
SELECT
{Column expression [ AS name ]} [ ,... ] | *
FROM <Table reference> [ {,<Table reference>} ... ]
[ WHERE search condition ]
[ORDER BY {col\_name | expr} [ASC | DESC],...]

Table reference can be either a table name (without alias)
represented as a string of english letters or a join clause.
Join clause:
  <Table reference> [JOIN TYPE] JOIN <Table reference> ON <join condition>
OR:
  <Table reference> CROSS JOIN <Table reference>

Only INNER and CROSS joins are interpreted at the moment, however all other types
are parsed and generated. OrderBy is only supported on the parser level.

Features done (append only):

- Parser of mini SQL
- Creation of databases from .csv files
- Query generation from AST
- Queries are generated with the following optimizations:
  - Predicates pushdown
  - Some kinds of predicates in filtration are transformed to joins
- Query interpretation
- Parser inline tests
- Parser cram tests
- Query generation cram tests
- Query interpretation (except orderby and left/right joins cause nulls are needed for them)

Features in progress (and TODOs):

- Write a HOWTO/USAGE
- Benchmark on big data
- Verification tests using some real database
- Improve error handling (especially in the catalog)
