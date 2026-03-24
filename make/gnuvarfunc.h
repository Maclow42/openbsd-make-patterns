#ifndef GNUVARFUNC_H
#define GNUVARFUNC_H

#include "defines.h"

extern bool GnuVar_ParseFunction(const char *, SymTable *, bool, size_t *,
    bool *, char **);
extern bool GnuVar_ParseFunctionSkip(const char *, size_t *);

#endif
