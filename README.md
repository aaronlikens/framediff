# framediff
This project started as a simple R translation of frame differencing Matlab code in Paxton and Dale (2012). In addition, we have added some auxillary functions for splitting video files into jpegs using a VERY SIMPLIFIED wrapper function for ffmpeg. We may add additional functionality for ffmpeg in the future but for now the function is single purposed towards extracting video frames for a user defined sample rate. In its current form, it can also extract only keyframes. There is also a function for estimating and plotting power spectral density in terms of both frequency and period. This may be useful for investigating frequency content of frame differencing output.

# installation
devtools::install_github('https://github.com/aaronlikens/framediff.git')
