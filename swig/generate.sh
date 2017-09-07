#!/bin/sh

#
# This script can be used to regenerate the bindings.lisp file using
# SWIG.
#

swig -cffi -module bindings -noswig-lisp -o ../driver/bindings.lisp bindings.i
# sed -i 's|(\([0-9]\+ [0-9]\+\))|\1|' bindings.lisp

# # ------------------------------------------------------------------------------
# # make our exports
# # ------------------------------------------------------------------------------
# echo -e "(in-package :nanomsg)\n" > exports.lisp
# cat bindings.lisp | \
#     grep -e '^(\(cffi\|cl\):' | \
#     grep -v 'defcstruct' | \
#     sed 's|^(cffi:defcfun.*" \(#.(lispify[^)]\+)\).*|\1|' | \
#     sed 's|^(cffi:defcenum.*\(#.(lispify[^)]\+)\).*|\1|' | \
#     sed 's|^(cffi:defcunion.*\(#.(lispify[^)]\+)\).*|\1|' | \
#     sed 's|^(cl:defconstant.*\(#.(lispify[^)]\+)\).*|\1|' | \
#     sed 's|^\(.*\)$|(export '"'"'\1)|' \
#     >> exports.lisp

# # ------------------------------------------------------------------------------
# # make our accessors
# # ------------------------------------------------------------------------------
# cat <<-EOFMAC > accessors.lisp
# (in-package :nanomsg.accessors)

# (defmacro make-accessors (c-struct)
#   \`(progn
#      ,@(loop for slot-name in (foreign-slot-names (intern (string c-struct) :nanomsg))
#              for accessor-name = (intern (concatenate 'string (symbol-name c-struct)
#                                                       "-"
#                                                       (symbol-name slot-name)))
#              append (list \`(defmacro ,accessor-name (ptr)
#                              (list 'foreign-slot-value ptr '',(intern (string c-struct) :nanomsg) '',slot-name))
#                           \`(export ',accessor-name :nanomsg.accessors)))))

# EOFMAC

# cat bindings.lisp | \
#     grep defcstruct | \
#     sed 's|.*#\.(lispify|(make-accessors #.(nanomsg::lispify|g' | sed 's|$|)|' \
#     >> accessors.lisp

# sed -i -e s/unsigned-int/int/ bindings.lisp
