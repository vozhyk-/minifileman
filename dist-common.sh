default_format=bz2

if [[ "$1" == "DEV" ]]
then
    suffix="dev`date +%Y%m%d.%H.%M`"
else
    suffix="$1"
fi
name="rest/dist/minifileman-${suffix}"
archive="${name}.tar"

if [[ "$2" == "" ]]
then
    format="$default_format"
else
    format="$2"
fi

case "$format" in
    gz)
        compressor=gzip
        ;;
    bz2)
        compressor=bzip2
        ;;
    lzma)
        compressor=lzma
        ;;
    *)
        echo The compression format $format isn\'t supported.
        exit 1
esac

dist_compress(){
    echo Compressing...
    ${compressor} -9v "${archive}"
}

dist_done(){
    echo Done.
}

echo Creating distribution tarball \`\`${archive}.${format}\'\'...
