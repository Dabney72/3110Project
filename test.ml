(** Our approach to testing was a mix of OUnit tests and play-testing the game.
    In general, we used glass-box testing with bisect to find missing OUnit
    tests and to make sure we hit all the branches of our code. We also did
    quite a bit of visual testing/playing the game to discover further bugs
    and test our program more.

    Below is a breakdown of how each module was tested:
    - state.ml: for this module, we used OUnit/bisect to automatically test
      all of the game mechanics that we had implemented. We also did a bit of
      manual testing by playing the game and catching bugs through it, and then
      writing OUnit tests to reproduce the bugs.
    - display.ml: for this module, we wrote a file with a bunch of helper
      functions to visually inspect that things were being drawn correctly on
      the screen.
    - move.ml, strategy.ml: these modules were primarily tested through OUnit
      and bisect, and we also watched the AI play on the GUI so we could see
      if it was behaving the way we wanted it to. This also resulted in
      finding further bugs and developing more OUnit tests. For strategies.ml,
      we had to do a bit more manual testing on the functions that had the AI
      train on random games, since we couldn't predict the properties of the
      output.
    - strategies.ml: due to the randomness of the output of the functions in
      this module, we could not run OUnit tests on it. Instead, we printed
      the output of the generation function and ensured that it was
      approaching a solution. In the end, we were able to get quite
      successful parameters for our AI, demonstrating that the generation
      function worked well.
    - tetromino.ml: again, we used OUnit and bisect to automatically test the
      different compositions and rotations of the tetrominoes. To develop
      test cases, we looked at every possible rotation of each tetromino type
      and created a test for it.
    - printers.ml: we checked for this module's correctness by inspecting the
      print statements we got in the console and making sure that they were
      formatted correctly. 

    For every module that we automatically tested with OUnit, we had over
    90% coverage, ensuring that the majority of our code has been activated
    by our test suites. We developed further test cases by repeatedly playing
    the game and trying to produce bugs, and the AI further helped by stress-
    testing the extreme cases. For example, our AI was able to run for about
    1 hour, clearing over 17,000 lines and getting a score of over 1,000,000
    without any errors/bugs. Due to the extensive OUnit and manual tests that
    we performed on our game, we feel that we have demonstrated the correctness
    of our system. *)

open Tetromino_test
open State_test
open Move_test
open Strategy_test