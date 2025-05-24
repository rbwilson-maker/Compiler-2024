#include "l1ckpt.h"
#include "nlohmann/json.hpp"

#include <cassert>
#include <fstream>

using namespace std;
using json = nlohmann::json;

json L1Ckpt::createEmptyDictionary() {
  return json::object();
}

json L1Ckpt::getLivenessInformation(char* input, int* target) {
  std::ifstream stream;
  stream.exceptions(std::ios::failbit | std::ios::badbit);
  stream.open(input);

  // Parse target k
  std::string targetStr;
  std::getline(stream, targetStr);
  if (target != nullptr) {
    size_t pos = targetStr.find("target ");
    assert(pos != std::string::npos);
    *target = atoi(targetStr.c_str() + pos + sizeof("target ") - 1);
  }

  json j;
  stream >> j;
  return j;
}

void L1Ckpt::outputAllocationInformation(char* input, json &out) {
  std::string file = std::string(input).substr(0, strlen(input) - sizeof(".in") + 1) + ".out";
  std::ofstream ofstream(file);
  ofstream << out;
}

void L1Ckpt::runWithLivenessInput(char* input) {
  int allocationTarget = -1;
  json livenessInformation = getLivenessInformation(input, &allocationTarget);

  // Perform allocation here
  (void)allocationTarget;
  (void)livenessInformation;
  assert(false && "Unimplemented");

  json outputAllocation;
  outputAllocationInformation(input, outputAllocation);
}

