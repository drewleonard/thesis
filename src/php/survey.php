<html>
 <head>
  <title>Survey</title>
 </head>
 <body>
 <?php

 // Settings
 header('Content-type: text/xml');
 setlocale(LC_MONETARY, 'en_US');

 // Number of images: total
 $nImgs = 3072;

 // Number of images: subset
 $nSlice = 10;

 // Get ids of images to pull
 $idsAll = range(1, $nImgs);
 shuffle($idsAll);
 $idsSubset = array_slice($idsAll, 0, $nSlice);

 // Build array of image tags (html)
 $imgTags = [];
 foreach($idsSubset as $id) {

 	$imgTag = '<img src="https://storage.cloud.google.com/thesis-ira-ads/ira_ad_'.$id.'.png"/>';
 	$imgTags[] = $imgTag;

 }

 $xmlResult = new SimpleXMLElement('<xml/>');
 $imgTagsChild = $xmlResult->addChild('imgTagsChild');

 foreach($imgTags as $imgTagN=>$imgTag) {
 	$imgTagsChild->addChild('imgTag'.$imgTagN, $imgTag);
 }

 ob_clean();
 print($xmlResult->asXML());
 $app->run();
 ?> 
 </body>
</html>
