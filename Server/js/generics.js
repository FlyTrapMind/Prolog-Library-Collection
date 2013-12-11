// jQuery uses  content type `application/x-form-encoded` for JSON data.
// I do not know why this is the case.
// @see http://peterstuifzand.nl/2008/09/05/jquery-and-postjson.html

function deleteJSON(url, data, callback) {
  $.ajax (
    url,
    {
      contentType: "application/json; charset=utf-8",
      data: JSON.stringify(data),
      dataType: "json",
      error: function(e) {console.log("exception: " + e.statusText);},
      success: callback,
      type: "delete"
    }
  );
}

function postJSON(url, data, callback) {
  postJSON(url, data, callback, {});
}

function postJSON(url, data, callback, headers) {
  $.ajax (
    url,
    {
      contentType: "application/json; charset=utf-8",
      data: JSON.stringify(data),
      dataType: "json",
      error: function(e) {console.log("exception: " + e.statusText);},
      headers: headers,
      success: callback,
      type: "post"
    }
  ).done(function() {
      console.log("done");
  });
}

function postJSON_auth(url, data, callback, auth) {
  postJSON(url, data, callback, {Authorization: "Basic " + auth});
}

// Notice that the response to an `$.ajax` request is already
// a JavaScript object.
// We do not need to explicitly parse the response text as JSON.

function successJSON(response) {
  var transform = {'tag':'p','html':'Message: ${msg}'};
  var html = json2html.transform(response, transform);
  $("#success").append(html);
}

