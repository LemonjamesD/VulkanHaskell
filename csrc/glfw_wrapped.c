#include <GLFW/glfw3.h>
#include "glfw_wrapped.h"
#include <string.h>
#include <stdlib.h>

const char* nullConstStr() {
  return (const char*)NULL;
}

const char* allocConstStr(char* str) {
  return (const char*)str;
}