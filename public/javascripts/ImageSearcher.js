ImageSearcher = function (queries, dom) {
    this.dom = dom;
    this.dom.find("button").hide();
    for (var i=0; i< queries.length; i++) {
        var query = queries[i];
        var imgDiv = ($('<span id="d'+i+'"></span>'));
        this.dom.append(imgDiv);
        this.searchImages(query, 0, 1, this.onSuccess.bind(this, imgDiv), this.onFail.bind(this));
    }
};

ImageSearcher.prototype.onSuccess = function (div, data, textStatus, jqXHR) {
    if (data.responseData) {
        var results = data.responseData.results;
        for (var i = 0; i < results.length; i++) {
            var result = results[i];
            div.append($('<img src="'+result.tbUrl+'"/>'));
        }
    }
};

ImageSearcher.prototype.onFail = function (jqXHR, textStatus, errorThrown) {
    console.error("Error getting images: " + textStatus + "\n" + errorThrown);
    // Put back images button
    this.dom.find("button").show();
};

ImageSearcher.prototype.searchImages = function (query, start, limit, successCallback, errorCallback) {
    // NOTE: image search has been deprecated, new custom search API limit to 100 queries a day
    var params = {
        v: "1.0",
        rsz: limit,
        safe: 'active',
        q: encodeURIComponent(query),
        start: start
    };
    var data = $.param(params);
    $.ajax({
        type: 'GET',
        url: 'https://ajax.googleapis.com/ajax/services/search/images',
        dataType: 'jsonp',
        data: data,
        imgsz: 'small|medium|large|xlarge|xxlarge|huge',
        success: successCallback,
        error: errorCallback
    });
};