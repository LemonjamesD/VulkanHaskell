#include <GLFW/glfw3.h>
#include "glfw_wrapped.h"
#include <string.h>
#include <stdlib.h>

int wrappedGlfwGetError(char* description) {
  const char* constDescription;
  int errorCode = glfwGetError(&constDescription);
  description = calloc(1, strlen(constDescription) + 1);
  if (constDescription != NULL) {
    strcpy(description, constDescription);
  }
  return errorCode;
}
