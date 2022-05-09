// Download tract-level CDD and HDD
// Written by: Anne Driscoll
// Last edited by: Jessica Li
// 
// Upload all_nationl_tracts shapefile as an asset to Google Earth Engine. 
// Run this in Google Earth Engine code editor.

var collection = ee.ImageCollection("OREGONSTATE/PRISM/AN81d")
                  .select('tmean')
                  .filterDate('1999-12-31', '2020-12-31')
                  
var us = ee.Geometry.Rectangle(-125.0,22.0,-66.6,48.6);

var sub_base_temp = function (image) {
  return image.select('tmean').subtract(18.3)
}
var fix_cdd = function (image) {
  // Mask all the pixels that are less than than 0, those are hdd
  return image.updateMask(image.lt(0))
}
var fix_hdd = function (image) {
  // Mask all the pixels that are greater than 0, those are cdd
  return image.updateMask(image.gt(0))
}


var subtracted = collection.map(sub_base_temp)

var cdd = subtracted.map(fix_cdd)
var hdd = subtracted.map(fix_hdd)

for (var i = 2000; i < 2021; i++) {

  var start_date = String(i).concat("-01-01")
  var end_date = String(i+1).concat("-01-01")

  var cur = collection.filterDate(start_date, end_date)
  var subtracted = cur.map(sub_base_temp)
  var cdd = subtracted.map(fix_cdd)
  var hdd = subtracted.map(fix_hdd)

  cdd = cdd.sum().abs() // now that it's subtracted, the cdd and hdd don't even out in the mean/sum
  hdd = hdd.sum().abs()

  cdd = cdd.reduceRegions({reducer: ee.Reducer.mean(),
                         collection: tracts,
                         scale: 3000});
  hdd = hdd.reduceRegions({reducer: ee.Reducer.mean(),
                           collection: tracts,
                           scale: 3000});

  Export.table.toDrive({
    collection: cdd, 
    description: "tract_level_cdd_".concat(i),
    fileFormat: 'CSV', 
    folder: "PRISM-temp", 
    selectors: ["GEOID", "mean"]
  })
  Export.table.toDrive({
    collection: hdd, 
    description: "tract_level_hdd_".concat(i),
    fileFormat: 'CSV', 
    folder: "PRISM-temp", 
    selectors: ["GEOID", "mean"]
  })

}
