<?php

// TODO 
// Number of images: total
$nImgs = 3500; // TODO: get

// Number of images: subset
$nSlice = 10;

// Get ids of images to pull
$idsAll = range(1, $nImgs);
shuffle($idsAll);
$idsSubset = array_slice($idsAll, 0, $nSlice);

// Set image properties
$imgHeight = 4400;
$imgWidth = 3400;

// Build array of image tags (html)
$imgTags = [];
foreach($idsSubset as $id) {

	$imgTag = '<img src="https://storage.cloud.google.com/thesis-ira-ads/ira_ad_'.$id.'.png" height="'.$imgHeight.'" width="'.$imgWidth.'" style="width:'.$imgWidth.'px; height:'.$imgHeight.'px;"/>';
	$imgTags[] = $imgTag;

}

$xmlResult = new SimpleXMLElement('<xml/>');
$imgTagsChild = $xmlResult->addChild('imgTagsChild');

foreach($imgTags as $imgTagN=>$imgTag) {

	$imgTagsChild->addChild('imgTag'.$imgTagN, $imgTag);

}

Header('Content-type: text/xml');
print($xmlResult->asXML());

?>