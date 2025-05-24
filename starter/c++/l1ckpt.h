#ifndef _L1CKPT_H_
#define _L1CKPT_H_

// Starter code utilizes nlohmann/json for parsing JSON. For information
// about how to use this library and some examples, refer to
// https://github.com/nlohmann/json#examples
#include "nlohmann/json_fwd.hpp"

using namespace std;

class L1Ckpt {
 public:
  /**
   * Run register allocation with the liveness information specified in input.
   * Function should write required output to file following the lab handout's
   * specification.
   *
   * @param input Input file containing liveness information
   * @returns void
   */
  static void runWithLivenessInput(char* input);

 private:
  /**
   * Load liveness information from a file. Liveness information is stored in
   * JSON format. For the structure of the json object, refer to the handout.
   *
   * @param input Input file
   * @param target Contains allocation target if non-null
   * @returns JSON object containing liveness information
   */
  static nlohmann::json getLivenessInformation(char* input, int* target);

  /**
   * Output allocation information to an output file following the handout.
   * For the structure of the output JSON object, refer to the handout.
   *
   * @param input Input file
   * @param out Output JSON object
   */
  static void outputAllocationInformation(char* input, nlohmann::json &out);

  /**
   * Creates an empty JSON dictionary object.
   */
  static nlohmann::json createEmptyDictionary();
};

#endif
