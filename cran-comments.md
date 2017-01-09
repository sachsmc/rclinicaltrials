This is a resubmission to address a critical bug: 

Kurt reported the following error. I was able to reproduce the error using travis CI. It is cause by something to do with the temporary directory created for the downloads. I don't exactly know what's causing it (doesn't occur in the tests) but I implemented a workaround by removing the "unlink(tmpdir)" calls. This led to another bug that meant old downloaded results were being used instead of the new ones. This update fixes that. 

