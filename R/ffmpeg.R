#' ffmpeg
#'
#' This function allows you to parse videos into individual frames
#'
#' @usage ffmpeg(path, filename, fps = NULL, keyframe = FALSE, outfile)
#' @param path character string specifies a path to a video to exported e.g.,
#' C:/Users/user/video_file_folder/
#' @param filename character string of file name including extension e.g.,
#' 'video.mp4'. Most video types are supported.
#' @param fps specifies the output frames per second (e.g., 29.97, 8, 1/5)
#' @param keyframe logical, if TRUE, fps is ignored and only keyframes are
#' returned. Hence, sampling rate may not be uniform.
#' @param outfile character string giving the output image prefix name
#' @export



ffmpeg = function(path, filename, fps = NULL, keyframe = FALSE, outfile){

    # check if R can find ffmpeg. If not, throw an error.
    ffmpeg_path = Sys.which('ffmpeg')
    if (nchar(ffmpeg_path) < 2){
        stop('R cannot find ffmpeg. Install ffmpeg or check path.')
    }

    # temporarily change working directory to chosen path
    old_dir = getwd()
    setwd(path)

    # create a directory to store images
    dir.create(outfile, showWarnings = FALSE)
    outname = ''

    # check if windows os and change path separators as needed
    if (Sys.info()[1] == 'Windows'){
        outname = paste(outfile,'\\',outfile, '_%05d.jpg', sep = '')
    }else{
        outname = paste(outfile,'/',outfile, '_%05d.jpg', sep = '')
    }

    # case 1: frame rate of video is known and all frames are requested
    # case 2: frame rate of video not known and all frames are requested
    # case 3: only keyframes (i.e., index frames) are requested
    if(!is.null(fps) & !keyframe){
        command = paste('ffmpeg -i ', filename, ' -vf fps=', fps, ' ', outname,
                        sep = '')
    }else if(is.null(fps) & keyframe == FALSE){
        command = paste('ffmpeg -i ', filename, ' ', outname,
                        sep = '')
    }else if(keyframe){
        command = paste('ffmpeg -i ', filename,
                        ' -vf \'select=eq(pict_type\\,I)\' ', '-vsync vfr ',
                        outname, sep = '')
    }

    # run ffmpeg command and reset working directory.
    system(command)
    setwd(old_dir)

}

