// jQuery uses  content type `application/x-form-encoded` for JSON data.
// I do not know why this is the case.
// @see http://peterstuifzand.nl/2008/09/05/jquery-and-postjson.html
function postJSON(url, data, callback) {
  $.ajax ({
    contentType: "application/json; charset=utf-8",
    data: JSON.stringify(data),
    dataType: "json",
    success: callback,
    type: "post",
    url: url
  });
}

