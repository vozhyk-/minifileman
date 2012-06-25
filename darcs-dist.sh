#!/bin/sh

source ./dist-common.sh

shift; shift
darcs_options="$@"

echo Running \`\`darcs dist ${darcs_options} -d "$name"\'\'...
darcs dist ${darcs_options} -d "$name"

if [[ "$format" != gz ]]
then
    echo Decompressing...
    gunzip -v "${archive}.gz"
    
    dist_compress
fi

dist_done
