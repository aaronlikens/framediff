
#' Power Spectral Density
#'
#' Estimates normalized power spectral density of a time series
#'
#' @usage power.spec(x, hz = 1, use.period = FALSE,
#' center = FALSE, detrend = FALSE, pad = TRUE,
#' logxy = FALSE, do.plot = TRUE, graph.par = NULL)
#' @param x a real valued time series
#' @param hz the sampling rate in Hz, default = 1
#' @param use.period logical indicating whether power should be given in terms of
#' period (s) instead of frequency (hz)
#' @param center logical indicating whether series should be centered
#' @param detrend logical indicating whether linear trend should be removed
#' @param pad logical indicating whether zero padding should be applied
#' @param logxy should frequency and power be returned in log10 units?
#' @param do.plot logical indicating whether plot should be generated. if
#' logxy = TRUE, plot will also be in log units.
#' @param graph.par a list comtaining additional graphical parameters to use for plotting.
#' The default is NULL
#' @return returns normalized power spectrum
#' @export
#' @examples
#' tt = (1:1000)/29.97
#' sintt = sin(2*pi*tt*3)
#' ps = power.spec(sintt, hz = 29.97,use.period = FALSE, center = TRUE, detrend = FALSE,
#'  pad = TRUE, logxy = FALSE, do.plot = TRUE)
#' @export
power.spec <- function(x, hz = 1, use.period = FALSE, center = FALSE, detrend = FALSE,
                       pad = TRUE, logxy = FALSE, do.plot = TRUE, graph.par = NULL){
    x = x
    if (pad){
        x = c(x, c(0,100))
    }
    N = length(x)
    tt = 1:length(x)/hz
    # check if series should be centered and perform centering
    if (center){
        x = x-mean(x)
    }

    # check if linear trend should be removed and remove it
    if (detrend){
        x = resid(lm(x ~ tt))
    }

    # create frequency vector
    freqz = seq(from = 0, to = hz/2, length.out = floor(N/2))
    period = 1/freqz
    xfft = 2*fft(x)/length(x)
    power = Mod(xfft)[1:length(freqz)]
    dominant_hz = freqz[which(power== max(power)) ]
    if (logxy){
        freqz = log10(freqz)
        power = log10(power)
        period = log10(period)
    }

    # change global graphical parameters based on graph.par if not NULL
    op = par(no.readonly = TRUE)
    if (!is.null(graph.par)){
        par(graph.par)
    }

    if(do.plot){
        if(use.period){
            # plot PSD omitting DC offset as a function of period
            plot(period[length(period):2], power[length(freqz):2],
                 xlab = 'Period (s)', ylab = 'Amplitude', type = 'l')
        }else{
            # plot PSD omitting DC offset as a function of frequency
            plot(freqz[2:length(period)], power[2:length(period)],
                 xlab = 'Frequency (Hz)', ylab = 'Amplitude', type = 'l',
                 cex.lab = 1.5, cex.axis = 1.5)
        }

    }


    out = list(freqz, period, power, dominant_hz)
    names(out) = c('Frequency','Period','PSD','Dominant.Frequency(Hz)')

    return(out)
}
