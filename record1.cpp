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

  int stopit = 0;

  IplImage* color_img;

  CvCapture* cv_cap = cvCaptureFromCAM(getopt('c', 0));

  cvNamedWindow("Video",0); // create window
  color_img = cvQueryFrame(cv_cap); // get frame
  int frameH    = getopt('h',(int) cvGetCaptureProperty(cv_cap, CV_CAP_PROP_FRAME_HEIGHT));
  cvSetCaptureProperty(cv_cap, CV_CAP_PROP_FRAME_HEIGHT, (double) frameH);
  int frameW    = getopt('w',(int) cvGetCaptureProperty(cv_cap, CV_CAP_PROP_FRAME_WIDTH));
  cvSetCaptureProperty(cv_cap, CV_CAP_PROP_FRAME_WIDTH, (double) frameW);
  int fps       = getopt('r',(int) cvGetCaptureProperty(cv_cap, CV_CAP_PROP_FPS));
  cvSetCaptureProperty(cv_cap, CV_CAP_PROP_FPS, (double) fps);
  //  int numFrames = (int) cvGetCaptureProperty(capture,  CV_CAP_PROP_FRAME_COUNT);

  //int fps     = 25;  // or 30
  //int frameW  = getopt('w', 800); // 744 for firewire cameras
  //int frameH  = getopt('h', 600); // 480 for firewire cameras

  //printf("framerate=%d\n",fps); fflush(stdout);

  int nframes = fps*60*getopt('m',60);
  int frcount=0, vidcount=0;

  char vidnm[80];

  CvVideoWriter *vid_write;
  printf("frames per movie = %d\n",nframes); fflush(stdout);

  for(;;) {
    sprintf(vidnm, "out%d.avi",vidcount);
    vid_write= cvCreateVideoWriter(vidnm,
				   //CV_FOURCC('M','J','P','G'),
				   CV_FOURCC('D','I','V','X'),
				   //CV_FOURCC('F','L','V','1'),
				   fps,cvSize(frameW,frameH),1);
    
    for(frcount = 0;frcount < nframes;frcount++) {
      color_img = cvQueryFrame(cv_cap); // get frame
      if(color_img != 0)
	cvShowImage("Video", color_img); // show frame
      c = cvWaitKey(10); // wait 10 ms or for key stroke
      //    printf("%d\n",c);
      
      if(c == 27) {
	stopit = 1;
	break; // if ESC, break and quit
      }
      cvWriteFrame( vid_write, color_img);
    }
    /* clean up */

    cvReleaseVideoWriter( &vid_write);
    if(stopit == 1) break;
    vidcount++;
  }
  cvReleaseCapture( &cv_cap );
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
