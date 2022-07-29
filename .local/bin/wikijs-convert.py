#!/bin/env python3

import argparse
import os
import sys
import datetime

FILTER = ["[[_TOC_]]", "{{< toc >}}"]

def add_wikijs_header_to_file(md_file, tags):
    with open(md_file, 'r') as f:
        lines = f.readlines()

    if len(lines) == 0:
        print('EmptyFile:', md_file)
        sys.exit(1)

    if not lines[0].startswith('---'):
        name = '.'.join(os.path.basename(md_file).split('.')[:-1])
        lines = [
                "---\n",
                "title: " + name + "\n",
                "description: " + name + "\n",
                "published: true\n",
                "date: " + datetime.datetime.now(datetime.timezone.utc).replace(tzinfo=None).replace(microsecond=0).isoformat() + '.000Z\n',
                "tags: " + tags + "\n",
                "editor: markdown\n",
                "dateCreated: " + datetime.datetime.now(datetime.timezone.utc).replace(tzinfo=None).replace(microsecond=0).isoformat() + '.000Z\n',
                "---\n",
                "\n"
                ] + lines

    lines = list(filter(lambda x: not any(txt in x for txt in FILTER), lines))

    with open(md_file, 'w') as f:
        f.writelines(lines)

parser = argparse.ArgumentParser()
parser.add_argument(dest='path', type=str, help="Path to markdown files directory")
parser.add_argument(dest='tags', type=str, help="File tags")
args = parser.parse_args()


if not os.path.exists(args.path):
    print('PathNotFound:', args.path)
    sys.exit(1)

markdonw_files = [os.path.join(args.path, f) for f in os.listdir(args.path) if f.lower().endswith(".md")]

for md_file in markdonw_files:
    add_wikijs_header_to_file(md_file, args.tags)

