// jQuery uses  content type `application/x-form-encoded` for JSON data.
// I do not know why this is the case.
// @see http://peterstuifzand.nl/2008/09/05/jquery-and-postjson.html

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
      headers: headers,
      success: callback,
      type: "post"
    }
  );
}

function postJSON_auth(url, data, callback, auth) {
  postJSON(url, data, callback, {Authorization: "Basic " + auth});
}

