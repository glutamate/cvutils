#include "opencv2/core/core.hpp"
#include "opencv2/imgproc/imgproc.hpp"
#include "opencv2/calib3d/calib3d.hpp"
#include "opencv2/highgui/highgui.hpp"
#include <stdio.h>

int getopt(char c, int defval);

int gargc;
char **gargv;

int main(int argc,char *argv[])
{
  gargc = argc;
  gargv = argv;

  int c;

  int stopit = 0;

  IplImage* color_img;

  double msec;
  CvCapture* cv_cap = cvCaptureFromFile(argv[1]);

  printf("height=%g\n", cvGetCaptureProperty(cv_cap, CV_CAP_PROP_FRAME_HEIGHT));
  printf("width=%g\n", cvGetCaptureProperty(cv_cap, CV_CAP_PROP_FRAME_WIDTH));
  printf("fps=%g\n", cvGetCaptureProperty(cv_cap, CV_CAP_PROP_FPS));
  printf("total frames=%g\n", cvGetCaptureProperty(cv_cap, CV_CAP_PROP_FRAME_COUNT));

  int frameid, frcount;
  sscanf(argv[2], "%d", &frameid); 

  cvGrabFrame(cv_cap); // get frame

  if(frameid>30) {
     cvSetCaptureProperty(cv_cap, CV_CAP_PROP_POS_FRAMES, frameid-20);
  }

  while(cvGetCaptureProperty(cv_cap, CV_CAP_PROP_POS_FRAMES)<(frameid-1)) {
    //for(frcount = 0;frcount < frameid;frcount++) {
    cvGrabFrame(cv_cap); // get frame
  }
//  cvGrabFrame(cv_cap); // get frame
//  printf("frameix=%g\n", cvGetCaptureProperty(cv_cap, CV_CAP_PROP_POS_FRAMES));
  color_img = cvQueryFrame(cv_cap); // get frame
  printf("frameix=%g\n", cvGetCaptureProperty(cv_cap, CV_CAP_PROP_POS_FRAMES));
  msec = cvGetCaptureProperty(cv_cap, CV_CAP_PROP_POS_MSEC);
  printf("msec=%g \n", msec);
  printf("sec=%g \n", msec/1000);
  cvSaveImage("extract.png",color_img);

  cvReleaseCapture( &cv_cap );
  return 0;
}

int getopt(char c, int defval) {
  int x, i;
  

    for (i = 1; i < gargc; i++)  /* Skip argv[0] (program name). */
    {
        /*
         * Use the 'strcmp' function to compare the argv values
         * to a string of your choice (here, it's the optional
         * argument "-q").  When strcmp returns 0, it means that the
         * two strings are identical.
         */

        if (gargv[i][1] == c)  /* Process optional arguments. */
        {
	  sscanf(gargv[i]+3, "%d", &x);
	  return x;
        }
    }
    return defval;
}
