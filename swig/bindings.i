%module bindings

/* %feature("intern_function", "lispify"); */

%insert("lisphead") %{
(in-package :cl-alsaseq)
%}

/* %{ */
/* #include <stdio.h> */
/* #include "stdlib.h" */
/* #include "string.h" */
/* #include "signal.h" */
/* #include "getopt.h" */
/* #include "sys/poll.h" */
/* #include "alsa/asoundlib.h" */
/* #include "aconfig.h" */
/* #include "version.h" */
/*   %} */

%include "stdint.i"
/* %include "stl.i" */
/* %include "cstring.i" */
/* %include "signal.h" */
/* %include "getopt.h" */
%include "poll.h"
%include "bits.poll.h"
/* %include "/usr/include/alsa/mixer.h" */
%include "seq_event.h"
%include "/usr/include/alsa/seq.h"
/* %include "/usr/include/alsa/mixer.h" */
%include "/usr/include/alsa/seqmid.h"
%include "/usr/include/alsa/seq_midi_event.h"

