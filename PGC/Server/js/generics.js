// jQuery uses  content type `application/x-form-encoded` for JSON data.
// I do not know why this is the case.
// @see http://peterstuifzand.nl/2008/09/05/jquery-and-postjson.html

// Notice that the response to an `$.ajax` request is already
// a JavaScript object.
// We do not need to explicitly parse the response text as JSON.



function deleteJSON(url) {
  $.ajax (
    url,
    {
      dataType: "json",
      error: errorJSON,
      success: successJSON,
      type: "delete"
    }
  );
}

function errorJSON(response) {
  var error = response[error];
  var message = response[message];
  var html = "<h3>" + error + "</h3><p>" + message + "</p>";
  $("#response").append(html);
}

function getJSON(url, headers) {
  if( headers == undefined ) headers = {}
  $.ajax (
    url,
    {
      contentType: "application/json; charset=utf-8",
      dataType: "json",
      error: errorJSON,
      headers: headers,
      success: successJSON,
      type: "GET"
    }
  );
}

function postJSON(url, data, headers) {
  if( headers == undefined ) headers = {}
  headers["Accept"] = "text/html; q=1.0";
  $.ajax (
    url,
    {
      contentType: "application/json; charset=utf-8",
      data: JSON.stringify(data),
      dataType: "json",
      error: errorJSON,
      headers: headers,
      success: successJSON,
      type: "POST"
    }
  );
}

function postJSON_auth(url, data, auth) {
  postJSON(url, data, {Authorization: "Basic " + auth});
}

function successJSON(response) {
  "use strict";
  var html;
  if (response.hasOwnProperty("message")) {
    html =
        json2html.transform(
          response,
          {"tag": "p", "html": "Message: ${message}"}
        );
  } else if (response.hasOwnProperty("error")) {
    html = "<h3>Error</h3><p>" + response.error + "</p>";
  } else {
    html = "<h3>Unknown success response</h3><p>" + response + "</p>";
  }
  $("#response").append(html);
}

