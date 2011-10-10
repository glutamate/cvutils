#include <cv.h>
#include <stdio.h>
#include <highgui.h>
#include "cv_help.h"
// globals

CvCapture* cv_cap;
IplImage* img;

// functions
void open_video(char *fnm) {
  cv_cap = cvCaptureFromFile(fnm);
  cvGrabFrame(cv_cap); 
  advance();
}

void advance() {
  img = cvQueryFrame(cv_cap); // get frame
}

void close_video() {
  cvReleaseCapture( &cv_cap );
}

uchar pixel_value(int x, int y, uchar ch) {
  return ((uchar*)(img->imageData + img->widthStep*y))[x*3+ch];
}

