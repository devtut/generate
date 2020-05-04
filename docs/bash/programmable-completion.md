---
metaTitle: "Programmable completion"
description: "Simple completion using function, Simple completion for options and filenames"
---

# Programmable completion



## Simple completion using function


```bash
_mycompletion() {
    local command_name="$1" # not used in this example
    local current_word="$2"
    local previous_word="$3" # not used in this example
    # COMPREPLY is an array which has to be filled with the possible completions
    # compgen is used to filter matching completions
    COMPREPLY=( $(compgen -W 'hello world' -- "$current_word") )
}    
complete -F _mycompletion mycommand

```

Usage Example:

```bash
$ mycommand [TAB][TAB]
hello world
$ mycommand h[TAB][TAB]
$ mycommand hello  

```



## Simple completion for options and filenames


```bash
# The following shell function will be used to generate completions for
# the "nuance_tune" command.
_nuance_tune_opts ()
{
  local curr_arg prev_arg
  curr_arg=${COMP_WORDS[COMP_CWORD]}
  prev_arg=${COMP_WORDS[COMP_CWORD-1]}

  # The "config" option takes a file arg, so get a list of the files in the
  # current dir.  A case statement is probably unnecessary here, but leaves
  # room to customize the parameters for other flags.
  case "$prev_arg" in
    -config)
      COMPREPLY=( $( /bin/ls -1 ) )
      return 0
      ;;
    esac
  
    # Use compgen to provide completions for all known options.
    COMPREPLY=( $(compgen -W '-analyze -experiment -generate_groups -compute_thresh -config -output -help -usage -force -lang -grammar_overrides -begin_date -end_date -group -dataset -multiparses -dump_records -no_index -confidencelevel -nrecs -dry_run -rec_scripts_only -save_temp -full_trc -single_session -verbose -ep -unsupervised -write_manifest -remap -noreparse -upload -reference -target -use_only_matching -histogram -stepsize' -- $curr_arg ) );
}

# The -o parameter tells Bash to process completions as filenames, where applicable.                                                                                                                                                        
complete -o filenames -F _nuance_tune_opts nuance_tune

```

