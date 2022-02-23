#!/usr/bin/env python
# -*- coding: utf-8 -*-

# About
# Add the subject of a mail as task to taskwarrior
#
# Usage
# add this to your .muttrc:
# macro index,pager t "<pipe-message>~/path/to/mutt2task.py<enter>" 

# import libraries
import sys
import email
from email.header import decode_header, make_header
import tasklib
                                                                                                                                                                                                         
# read from stdin and parse subject
e = sys.stdin.read()
e = email.message_from_string(e)
e = e['Subject']

# decode internationalized subject
e = decode_header(e)
# e = [(subject,encoding)]
e = ' '.join([i[0].decode(i[1] or 'utf-8') for i in e])

# Add to taskwarrior
tw = tasklib.TaskWarrior()
task = tasklib.Task(tw)
task["description"] = e
task["tags"] = ["email"]
task.save()
