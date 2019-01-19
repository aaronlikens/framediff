#' Frame Differencing
#'
#' Performs frame differencing on a sequence of video frames
#' @usage fd(dir, m = 2, cutoff = 1, do.plot = TRUE,
#' split_screen = TRUE, verbose = FALSE, shuffle = FALSE,
#' mag = TRUE)
#' @param dir dir specifies a directory containing sequentially numbered images.
#'  Uses current directory by default.
#' @param do.filter logical indicating whether filtering should be performed
#' @param m integer order of the butterworth filter
#' @param cutoff real-valued frequency spectrum cutoff proportion for
#' butterworth filter
#' @param do.plot logical indicating whether a plot should be generated
#' @param split_screen logical indicating whether frame differencing should
#' be applied separately to each 1/2 of a screen.
#' @param verbose logical indicating whether feedback about which frame is
#' being anlayzed
#' @param shuffle logical indicating whether image order should be shuffled
#' for the purpose of surrogate analysis
#' @param mag logical indicating whether magnitude (absolute frame difference)
#' or difference should be returned. Default is TRUE
#' @importFrom jpeg readJPEG
#' @importFrom signal butter filter
#' @importFrom graphics lines par plot
#' @importFrom stats fft lm resid
#' @export


fd = function(dir = NULL, m = 2, cutoff = 1, do.plot = TRUE,
              split_screen = TRUE, verbose = FALSE, shuffle = FALSE,
              mag = TRUE){

    # check user input and use the specified directory. Otherwise current
    # directory will be used.
    if (!is.null(dir)){
        setwd(dir)
    }
    # fetch image filenames and make sure there are some
    filelist = list.files(pattern = '*.jpg')
    if (length(filelist) < 2){
        stop('Not enough images found in the specified/current directory.')
    }
    if (shuffle){
        filelist = sample(filelist, replace = F)
    }

    # create vectors for output data
    image_z_diffs = NULL
    plms = rep(NA, length(filelist))
    prms = rep(NA, length(filelist))
    counter = 1
    for (i in 2:length(filelist)){
        if (verbose){
            cat('Processing Image: ', i, '\n')
        }


        # prep the files
        file_name = filelist[i]
        image_2 = jpeg::readJPEG(file_name)
        file_name = filelist[i - 1]
        image_1 = jpeg::readJPEG(file_name)

        # collappse across color
        image_2 = rowMeans(image_2, dims = 2)
        image_2[is.nan(image_2)] = NULL
        image_1 = rowMeans(image_1, dims = 2)
        image_1[is.nan(image_1)] = NULL

        # turn images into pixel z-scores
        image_2 = scale(image_2)
        image_1 = scale(image_1)

        # difference, standardize, and store difference vectors
        image_diff = matrix(0, nrow(image_2), nrow(image_1))
        if (mag){
            image_diff = abs(image_2 - image_1)
        }else{
            image_diff = image_2 - image_1
        }


        # check and split images into left and right if required
        left = floor(ncol(image_diff)/2)
        right = ncol(image_diff)
        if (split_screen){
            plms[counter] = mean(image_diff[,1:left])
            prms[counter] = mean(image_diff[,(left+1):right])
        }else{
        # average over entire image if no split is required
            plms[counter] = mean(image_diff[,1:right])
            prms[counter] = plms[counter]
        }

        # increment index counter
        counter = counter + 1

    }
    l = NULL
    r = NULL
    if (do.filter){
        # smooth signal with a butter worth filter e.g., Paxton and Dale (2012)
        bf = signal::butter(m, cutoff)
        l = signal::filter(bf, plms)
        r = signal::filter(bf, prms)
    }


    # format output data
    out = data.frame(l, r)
    names(out) = c('left', 'right')

    # plot data if desired
    if(do.plot){
        ymin = min(min(l), min(r))
        ymax = max(max(l), max(r))
        plot(1:nrow(out), out$left, type = 'l', col = 'blue',
             xlab = 'Frame', ylab = 'Magnitude', ylim = c(ymin, ymax))
        lines(1:nrow(out), out$right)
    }

    return(out)

}
