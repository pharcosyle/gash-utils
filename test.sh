cat <<EOF
foobar
EOF

for file in $(find * -type f)
do
  if [ "${file}" != "generator.log"\
       -a "${file}" != "gaiag.log"\
       -a "${file}" != "${basename}.scm"\
       -a "`basename ${file} .dzn`.dzn" != "${file}" ]
  then
    filecount=$((filecount+1))
    #files[${filecount}]=${file}
  fi
done

echo foo 2>&1
ls -l / -1

(echo; echo)

if true
then
    echo
fi

for f in foo; do echo; done

for file in *.im
do
  ${bin}/asd -l gen2 ${file} 2>&1 codegenerator.log || error "codegenerator gen2 failure: ${file}" codegenerator.log
done

cat foo || echo ok && echo nok

foo=$*
foo=$@
foo=$(dirname $(dirname $@))

foo || bar && baz

${bin}/generate -p componentfile.dzn > pretty.dzn 2> pretty.err && cat pretty.dzn || cat componentfile.dzn

filecount=-1

if [ "${file}" != "generator.log"\
               -a "${file}" != "gaiag.log"\
               -a "${file}" != "${basename}.scm"\
               -a "`basename ${file} .dzn`.dzn" != "${file}" ]
then
    echo
    filecount=$((filecount+1))
    #files[${filecount}]=${file}
fi


for file in $(find * -type f)
do
    echo
done

for file in $(find * -type f)
do
    echo $file
done

if ls& ls; then echo foo& echo bar || echo foo; echo barf; fi

for f in foo bar; do echo; done
ls

model=$1
model=

if [ "${model}" = "" ]
then
    echo
fi
