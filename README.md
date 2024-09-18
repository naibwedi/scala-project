# Scala Assignment

For this project you are to deliver your proposed solution for solving the puzzle. 
Once delivered, you can run your code using [Bamboo](https://tools.uia.no/bamboo/browse/IKT212G23H-SCALA/branches), which runs the validation test.
You can find the validation test puzzles in [teacher-tests/ValidationData](https://tools.uia.no/bitbucket/projects/IKT212G23H/repos/teacher-tests/browse/ValidationData).

### Project Structure

To run the tests in Bamboo, a specific structure is expected (see below). The root folder has to be named "ScalaAssignment", in which all the Scala files are located. 
The main Scala file has to be named "PuzzleSolver.scala". This file will be executed in bamboo, and as such must be present.
The main Scala file is passed two command line parameters for the file to read (the puzzle) and the file to write (the solution).
There should be a file describing the internal structure for the puzzle and the solution, for example "Puzzle.scala".
In addition, all file writing and reading is to be done in a file "PuzzleReaderWriter.scala" only.
This means that the reader and the writer translate between the internal structure as defined in "Puzzle.scala" and the text input and output.
You are free to add any other Scala files.

```sh
    |-ScalaAssignment
    |--Puzzle.scala
    |--PuzzleReaderWriter.scala
    |--PuzzleSolver.scala
    |--*.scala
```

The ReaderWriter file is in preparation for the next project, so make sure to be able to replace the file reading and writing by exchanging the "PuzzleReaderWriter.scala".
Please see the sample files here for a trivial example for how to process puzzles.

The input and output format and file names are explained in Canvas. Make sure to follow the conventions.
