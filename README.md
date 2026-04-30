# Liminale

This project started with the aim of algorithmically generating music for meditation or background listening. While I do not focus on utilizing any particular frequencies, synthesizing audio from code allows me to work with exact frequency ratios without the need for any tuning system. This project is as much a joke as it is a sincere exploration of music outside my usual habits, as well as a fun coding exercise.

Further more, the algorithms I developed for generating new durations and frequencies might be usefull in other contexts. To use liminale for your own project, you might want to define a new note-type, adjust its parameters and overload the #'generate-new-notes function for that type. Check out the src/piano.lsp file for inspiration.

## Dependencies

For liminale I am using the [CLM](https://ccrma.stanford.edu/software/clm/) library for synthesis, however it is possible to use liminale withouth CLM. I am also using my own [layers-utils](https://github.com/Leon-Focker/layers-utils) library.

## Install

Clone [this repository](https://https://github.com/Leon-Focker/liminale) or download the source code (Beware that this also clones the .wav files in the samples/ folder via lfs. If you don't need them, want to disable lfs.). Then either:

**Option 1:** Place the folder in your [Quicklisp local-projects directory](http://blog.quicklisp.org/2018/01/the-quicklisp-local-projects-mechanism.html), for example:
```
~/quicklisp/local-projects/
```

**Option 2:** Add the path manually in your Lisp session:
```
(push "/full/path/to/liminale/" asdf:*central-registry*)
```

Then load the system with:
```
(ql:quickload :liminale)
```

## Use

Once installed, you can just run src/liminale.lsp to generate music. To configer this for your own needs, check out how to define your own note-types with #'define-note-type. An example for this and midi file generation can be found in src/piano.lsp.