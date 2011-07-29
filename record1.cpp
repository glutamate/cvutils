#include <cv.h>
#include <stdio.h>
#include <highgui.h>

int getopt(char c, int defval);

int gargc;
char **gargv;

int main(int argc,char *argv[])
{
  gargc = argc;
  gargv = argv;

  int c;
  IplImage* color_img;
  CvCapture* cv_cap = cvCaptureFromCAM(0);

  cvNamedWindow("Video",0); // create window
  color_img = cvQueryFrame(cv_cap); // get frame
  int frameH    = (int) cvGetCaptureProperty(cv_cap, CV_CAP_PROP_FRAME_HEIGHT);
  int frameW    = (int) cvGetCaptureProperty(cv_cap, CV_CAP_PROP_FRAME_WIDTH);
  int fps       = (int) cvGetCaptureProperty(cv_cap, CV_CAP_PROP_FPS);
  //  int numFrames = (int) cvGetCaptureProperty(capture,  CV_CAP_PROP_FRAME_COUNT);

  //int fps     = 25;  // or 30
  //int frameW  = getopt('w', 800); // 744 for firewire cameras
  //int frameH  = getopt('h', 600); // 480 for firewire cameras

  CvVideoWriter *vid_write = cvCreateVideoWriter("out.avi",CV_FOURCC('D','I','V','X'),
						 fps,cvSize(frameW,frameH),1);

  for(;;) {
    color_img = cvQueryFrame(cv_cap); // get frame
    if(color_img != 0)
      cvShowImage("Video", color_img); // show frame
    c = cvWaitKey(10); // wait 10 ms or for key stroke
    //    printf("%d\n",c);

    if(c == 27)
      break; // if ESC, break and quit
    cvWriteFrame( vid_write, color_img);
  }
  /* clean up */
  cvReleaseCapture( &cv_cap );
  cvReleaseVideoWriter( &vid_write);
  cvDestroyWindow("Video");
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
