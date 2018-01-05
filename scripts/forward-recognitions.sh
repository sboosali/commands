#!/bin/bash

# $ ./scripts/forward-recognitions.sh

#########################################

SHARED_FOLDER="/home/sboo/guest_IE11_Win7"

#########################################
# watch a file

# doesn't work, doesn't see 

TRANSCRIPTION_FILENAME=transcription
WATCHED_FILE="$SHARED_FOLDER/$TRANSCRIPTION_FILENAME"

# inotifywait --monitor --event 'modify' "$WATCHED_FILE" | while read -r filename event; do
#   cat "${filename}" 
# done

# inotifywait --monitor --quiet --event 'modify' "$WATCHED_FILE" | while read -r filename event; do
#   cat "${filename}" 
# done

# inotifywait "$SHARED_FOLDER/$TRANSCRIPTION_FILE" --monitor --event 'modify'

#########################################
# watch a directory

# triggers by any file in that directory being written to 
# (a newly created file that's nonempty triggers both `create` and `modify`)

TRANSCRIPTIONS_DIRECTORY=transcriptions
WATCHED_DIRECTORY="$SHARED_FOLDER/$TRANSCRIPTIONS_DIRECTORY/"

# excludes dotfiles, files that start with a dot
# in particular, ephemeral emacs files like: /home/sboo/guest_IE11_Win7/transcriptions/.#-emacsa05488

# TODO  --exclude '\..+' matches a dot anywhere in string, and '\`' doesn't work

# also see https://stackoverflow.com/questions/7943528/inotifywait-exclude-regex-pattern-formatting

echo Watching "$WATCHED_DIRECTORY"
echo

inotifywait --monitor --exclude '\..+' --event 'modify' $WATCHED_DIRECTORY | grep "${WATCHED_DIRECTORY}.*${TRANSCRIPTION_FILENAME}" --line-buffered | while read -r directory event filename; do
  echo "----------------------------------------"
  echo "${event}"
  cat "${directory}${filename}" 
  echo
done

# in emacs, transcribe into subdir for inotifywait

#########################################

# NAME
#        inotifywait - wait for changes to files using inotify
#        inotifywait  is  part  of  inotify-tools.

# SYNOPSIS
#        inotifywait  [-hcmrq]  [-e  <event> ] [-t <seconds> ] [--format <fmt> ]
#        [--timefmt <fmt> ] <file> [ ... ]

# OUTPUT
#        inotifywait  will  output  diagnostic information on standard error and
#        event information on standard output.  The event output can be  config‐
#        ured, but by default it consists of lines of the following form:

#        watched_filename EVENT_NAMES event_filename

#        watched_filename
#               is  the  name  of  the file on which the event occurred.  If the
#               file is a directory, a trailing slash is output.

#        EVENT_NAMES
#               are the names of the inotify events which occurred, separated by
#               commas.

#        event_filename
#               is  output  only  when the event occurred on a directory, and in
#               this case the name of the file within the directory which caused
#               this event is output.              

# --outfile <file>
#               Output events to <file> rather than stdout.

# --monitor
#               Instead of exiting  after  receiving  a  single  event,  execute
#               indefinitely. 

# --recursive
#               Watch all subdirectories of any directories passed as arguments.
#               Watches will be set up recursively to an unlimited depth.   Sym‐
#               bolic  links  are  not  traversed.  Newly created subdirectories
#               will also be watched.

# --event <event>
#               Listen for specific event(s) only.  The events which can be lis‐
#               tened  for are listed in the EVENTS section.  This option can be
#               specified more than once.  If omitted, all events  are  listened
#               for.

# --exclude <pattern>
#        Do not process any events whose  filename  matches
#        the  specified  POSIX extended regular expression,
#        case sensitive.

# EVENTS
#        The following events are valid for use with the -e option:

#        access A  watched  file  or  a file within a watched directory was read
#               from.

#        modify A watched file or a file within a watched directory was  written
#               to.

#        attrib The metadata of a watched file or a file within a watched direc‐
#               tory was modified.  This includes timestamps, file  permissions,
#               extended attributes etc.

#        close_write
#               A  watched file or a file within a watched directory was closed,
#               after being opened in writeable mode.  This does not necessarily
#               imply the file was written to.

#        close_nowrite
#               A  watched file or a file within a watched directory was closed,
#               after being opened in read-only mode.

#        close  A watched file or a file within a watched directory was  closed,
#               regardless  of  how  it  was opened.  Note that this is actually
#               implemented  simply  by  listening  for  both  close_write   and
#               close_nowrite, hence all close events received will be output as
#               one of these, not CLOSE.

#        open   A watched file or a file within a watched directory was opened.

#        moved_to
#               A file or directory was moved into a  watched  directory.   This
#               event  occurs  even  if the file is simply moved from and to the
#               same directory.

#        moved_from
#               A file or directory was moved from a  watched  directory.   This
#               event  occurs  even  if the file is simply moved from and to the
#               same directory.

#        move   A file or directory was moved from or to  a  watched  directory.
#               Note  that  this is actually implemented simply by listening for
#               both moved_to and moved_from, hence all  close  events  received
#               will be output as one or both of these, not MOVE.

#        move_self
#               A  watched  file  or  directory was moved. After this event, the
#               file or directory is no longer being watched.

#        create A file or directory was created within a watched directory.

#        delete A file or directory within a watched directory was deleted.

#        delete_self
#               A watched file or directory was deleted.  After this  event  the
#               file  or  directory  is no longer being watched.  Note that this
#               event can occur even if it is not explicitly being listened for.

#        unmount
#               The filesystem on which a watched file or directory resides  was
#               unmounted.   After this event the file or directory is no longer
#               being watched.  Note that this event can occur even if it is not
#               explicitly being listened to.

#########################################