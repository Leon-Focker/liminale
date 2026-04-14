# Liminale

This is a small project aimed at algorithmically generating music for meditation or background listening. While I do not focus on utilizing any particular frequencies, synthesizing audio from code allows me to work with exact frequency ratios without the need for any tuning system. This project is as much a joke as it is a sincere exploration of music outside my usual habits, as well as a fun coding exercise.

For liminale I am using the [CLM](https://ccrma.stanford.edu/software/clm/) library for synthesis and my own [layers-utils](https://github.com/Leon-Focker/layers-utils) library.

If you want to run this for yourself, you'll need to use your own samples. All sample paths are configuered within src/synths.lsp. Note however, that I tuned the synthesis for my samples. The generation of the structures and soundfiles has to be called manually from src/main.lsp.

