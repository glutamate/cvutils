#include <cv.h>
#include <stdio.h>
#include <highgui.h>
int main(int argc,char *argv[])
{
int c;
IplImage* color_img;
CvCapture* cv_cap = cvCaptureFromCAM(0);
cvNamedWindow("Video",0); // create window
for(;;) {
color_img = cvQueryFrame(cv_cap); // get frame
if(color_img != 0)
cvShowImage("Video", color_img); // show frame
c = cvWaitKey(10); // wait 10 ms or for key stroke
printf("%d\n",c);

if(c == 27)
break; // if ESC, break and quit
}
/* clean up */
cvReleaseCapture( &cv_cap );
cvDestroyWindow("Video");
return 0;
}
