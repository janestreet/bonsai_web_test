open! Core
module Soup = Lambdasoup

let not_self_closing = [ "textarea"; "option" ]

let soup_to_string soup =
  let buf = Buffer.create 1024 in
  let rec loop soup =
    match Soup.element soup with
    | Some element ->
      let tag = Soup.name element in
      bprintf buf "<%s" tag;
      Soup.fold_attributes
        (fun () name value ->
          match value with
          | "" -> bprintf buf " %s" name
          | _ -> bprintf buf " %s=\"%s\"" name value)
        ()
        element;
      if Soup.no_children element
      then
        if List.mem not_self_closing tag ~equal:String.equal
        then bprintf buf "></%s>" tag
        else bprintf buf " />"
      else (
        bprintf buf ">";
        Soup.children element |> Soup.iter loop;
        bprintf buf "</%s>" tag)
    | None ->
      (match Soup.texts soup with
       | [] -> ()
       | [ text ] -> bprintf buf "%s" text
       | _ -> raise_s [%message "Node that isn't an element must be a text node."])
  in
  loop soup;
  Buffer.contents buf
;;

let minify_html_string html =
  String.strip html
  |> String.split ~on:'\n'
  |> List.map ~f:String.strip
  |> String.concat ~sep:" "
  |> String.strip
;;

let test html =
  let soup = Lambda_soup_js.parse html in
  match Soup.select_one "body" soup with
  | None -> failwith "No body"
  | Some body ->
    (match Soup.child body with
     | None -> ()
     | Some child ->
       let output = soup_to_string child in
       let minified_input = minify_html_string html in
       let minified_output = minify_html_string output in
       let diff = Expect_test_patdiff.patdiff html output in
       if String.equal minified_input minified_output
          || String.is_empty (String.strip diff)
       then print_endline "No diff detected between minified input and output"
       else (
         print_endline "####### DIFF -INPUT +OUTPUT ########\n";
         print_endline diff);
       print_endline "\n===========OUTPUT BELOW=============\n";
       print_endline output)
;;

let%expect_test "Empty" =
  test "";
  [%expect {| |}]
;;

let%expect_test "Basic paragraph" =
  test {|<p>Hello, world!</p>|};
  [%expect
    {|
    No diff detected between minified input and output

    ===========OUTPUT BELOW=============

    <p>Hello, world!</p>
    |}]
;;

let%expect_test "Heading with class and id" =
  test {|<h1 class="title" id="main-title">Welcome</h1>|};
  [%expect
    {|
    No diff detected between minified input and output

    ===========OUTPUT BELOW=============

    <h1 class="title" id="main-title">Welcome</h1>
    |}]
;;

let%expect_test "Unordered list with custom attributes" =
  test
    {|
    <ul data-list-type="custom">
      <li data-item="1">Item 1</li>
      <li data-item="2">Item 2</li>
      <li data-item="3">Item 3</li>
    </ul>
  |};
  [%expect
    {|
    No diff detected between minified input and output

    ===========OUTPUT BELOW=============

    <ul data-list-type="custom">
          <li data-item="1">Item 1</li>
          <li data-item="2">Item 2</li>
          <li data-item="3">Item 3</li>
        </ul>
    |}]
;;

let%expect_test "Image with srcset and sizes" =
  test
    {|
    <img src="image-small.jpg"
         srcset="image-small.jpg 300w, image-medium.jpg 600w, image-large.jpg 1200w"
         sizes="(max-width: 600px) 300px, (max-width: 1200px) 600px, 1200px"
         alt="A responsive image" />
  |};
  [%expect
    {|
    No diff detected between minified input and output

    ===========OUTPUT BELOW=============

    <img src="image-small.jpg" srcset="image-small.jpg 300w, image-medium.jpg 600w, image-large.jpg 1200w" sizes="(max-width: 600px) 300px, (max-width: 1200px) 600px, 1200px" alt="A responsive image" />
    |}]
;;

let%expect_test "Anchor with rel and download attributes" =
  test
    {|
    <a href="https://example.com/document.pdf"
       target="_blank"
       rel="noopener noreferrer"
       download="example-document.pdf">
      Download PDF
    </a>
  |};
  [%expect
    {|
    No diff detected between minified input and output

    ===========OUTPUT BELOW=============

    <a href="https://example.com/document.pdf" target="_blank" rel="noopener noreferrer" download="example-document.pdf">
          Download PDF
        </a>
    |}]
;;

let%expect_test "Table with colspan and rowspan" =
  test
    {|
    <table>
      <thead>
        <tr>
          <th colspan="2">Name</th>
          <th>Age</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td>John</td>
          <td>Doe</td>
          <td rowspan="2">30</td>
        </tr>
        <tr>
          <td>Jane</td>
          <td>Doe</td>
        </tr>
      </tbody>
    </table>
  |};
  [%expect
    {|
    No diff detected between minified input and output

    ===========OUTPUT BELOW=============

    <table>
          <thead>
            <tr>
              <th colspan="2">Name</th>
              <th>Age</th>
            </tr>
          </thead>
          <tbody>
            <tr>
              <td>John</td>
              <td>Doe</td>
              <td rowspan="2">30</td>
            </tr>
            <tr>
              <td>Jane</td>
              <td>Doe</td>
            </tr>
          </tbody>
        </table>
    |}]
;;

let%expect_test "Form with various input types and attributes" =
  test
    {|
    <form action="/submit" method="post">
      <input type="text" name="username" required placeholder="Username" />
      <input type="password" name="password" required minlength="8" />
      <input type="email" name="email" autocomplete="email" />
      <input type="tel" name="phone" pattern="[0-9]{3}-[0-9]{3}-[0-9]{4}" />
      <input type="submit" value="Login" disabled />
    </form>
  |};
  [%expect
    {|
    No diff detected between minified input and output

    ===========OUTPUT BELOW=============

    <form action="/submit" method="post">
          <input type="text" name="username" required placeholder="Username" />
          <input type="password" name="password" required minlength="8" />
          <input type="email" name="email" autocomplete="email" />
          <input type="tel" name="phone" pattern="[0-9]{3}-[0-9]{3}-[0-9]{4}" />
          <input type="submit" value="Login" disabled />
        </form>
    |}]
;;

let%expect_test "Div with data attributes and aria roles" =
  test
    {|
    <div class="container" data-user-id="123" role="main" aria-label="Main content">
      <h2>Section Title</h2>
      <p>This is a paragraph inside a div.</p>
    </div>
  |};
  [%expect
    {|
    No diff detected between minified input and output

    ===========OUTPUT BELOW=============

    <div class="container" data-user-id="123" role="main" aria-label="Main content">
          <h2>Section Title</h2>
          <p>This is a paragraph inside a div.</p>
        </div>
    |}]
;;

let%expect_test "Ordered list with reversed attribute" =
  test
    {|
    <ol start="5" reversed>
      <li value="5">Fifth item</li>
      <li>Fourth item</li>
      <li>Third item</li>
    </ol>
  |};
  [%expect
    {|
    No diff detected between minified input and output

    ===========OUTPUT BELOW=============

    <ol start="5" reversed>
          <li value="5">Fifth item</li>
          <li>Fourth item</li>
          <li>Third item</li>
        </ol>
    |}]
;;

let%expect_test "Button with custom data attributes" =
  test
    {|
    <button
      onclick="alert('Hello!')"
      data-action="greet"
      data-message="Welcome"
      aria-pressed="false">
      Click me
    </button>
  |};
  [%expect
    {|
    No diff detected between minified input and output

    ===========OUTPUT BELOW=============

    <button onclick="alert('Hello!')" data-action="greet" data-message="Welcome" aria-pressed="false">
          Click me
        </button>
    |}]
;;

let%expect_test "Textarea with maxlength and readonly" =
  test
    {|
    <textarea
      rows="4"
      cols="50"
      maxlength="200"
      readonly
      placeholder="This text cannot be edited">
      Predefined content goes here.
    </textarea>
  |};
  [%expect
    {|
    No diff detected between minified input and output

    ===========OUTPUT BELOW=============

    <textarea rows="4" cols="50" maxlength="200" readonly placeholder="This text cannot be edited">      Predefined content goes here.
        </textarea>
    |}]
;;

let%expect_test "Select dropdown with optgroup" =
  test
    {|
    <select name="cars">
      <optgroup label="Swedish Cars">
        <option value="volvo">Volvo</option>
        <option value="saab">Saab</option>
      </optgroup>
      <optgroup label="German Cars">
        <option value="mercedes">Mercedes</option>
        <option value="audi" selected>Audi</option>
      </optgroup>
    </select>
  |};
  [%expect
    {|
    No diff detected between minified input and output

    ===========OUTPUT BELOW=============

    <select name="cars">
          <optgroup label="Swedish Cars">
            <option value="volvo">Volvo</option>
            <option value="saab">Saab</option>
          </optgroup>
          <optgroup label="German Cars">
            <option value="mercedes">Mercedes</option>
            <option value="audi" selected>Audi</option>
          </optgroup>
        </select>
    |}]
;;

let%expect_test "Fieldset with disabled attribute" =
  test
    {|
    <fieldset disabled>
      <legend>Personal Information</legend>
      <label for="name">Name:</label>
      <input type="text" id="name" name="name" />
      <label for="email">Email:</label>
      <input type="email" id="email" name="email" />
    </fieldset>
  |};
  [%expect
    {|
    No diff detected between minified input and output

    ===========OUTPUT BELOW=============

    <fieldset disabled>
          <legend>Personal Information</legend>
          <label for="name">Name:</label>
          <input type="text" id="name" name="name" />
          <label for="email">Email:</label>
          <input type="email" id="email" name="email" />
        </fieldset>
    |}]
;;

let%expect_test "Article with time and address elements" =
  test
    {|
    <article itemscope itemtype="http://schema.org/BlogPosting">
      <header>
        <h1 itemprop="headline">Article Title</h1>
        <p>Posted on <time itemprop="datePublished" datetime="2023-04-22T15:30:00+00:00">April 22, 2023</time></p>
      </header>
      <p itemprop="articleBody">Article content goes here.</p>
      <footer>
        <address itemprop="author" itemscope itemtype="http://schema.org/Person">
          Author: <span itemprop="name">John Doe</span>
        </address>
      </footer>
    </article>
  |};
  [%expect
    {|
    No diff detected between minified input and output

    ===========OUTPUT BELOW=============

    <article itemscope itemtype="http://schema.org/BlogPosting">
          <header>
            <h1 itemprop="headline">Article Title</h1>
            <p>Posted on <time itemprop="datePublished" datetime="2023-04-22T15:30:00+00:00">April 22, 2023</time></p>
          </header>
          <p itemprop="articleBody">Article content goes here.</p>
          <footer>
            <address itemprop="author" itemscope itemtype="http://schema.org/Person">
              Author: <span itemprop="name">John Doe</span>
            </address>
          </footer>
        </article>
    |}]
;;

let%expect_test "Navigation with ARIA attributes" =
  test
    {|
    <nav role="navigation" aria-label="Main menu">
      <ul>
        <li><a href="#home" aria-current="page">Home</a></li>
        <li><a href="#about">About</a></li>
        <li><a href="#contact">Contact</a></li>
      </ul>
    </nav>
  |};
  [%expect
    {|
    No diff detected between minified input and output

    ===========OUTPUT BELOW=============

    <nav role="navigation" aria-label="Main menu">
          <ul>
            <li><a href="#home" aria-current="page">Home</a></li>
            <li><a href="#about">About</a></li>
            <li><a href="#contact">Contact</a></li>
          </ul>
        </nav>
    |}]
;;

let%expect_test "Audio player with multiple sources and fallback" =
  test
    {|
    <audio controls preload="auto">
      <source src="audio.ogg" type="audio/ogg" />
      <source src="audio.mp3" type="audio/mpeg" />
      <track kind="captions" src="captions.vtt" srclang="en" label="English" />
      Your browser does not support the audio element.
    </audio>
  |};
  [%expect
    {|
    No diff detected between minified input and output

    ===========OUTPUT BELOW=============

    <audio controls preload="auto">
          <source src="audio.ogg" type="audio/ogg" />
          <source src="audio.mp3" type="audio/mpeg" />
          <track kind="captions" src="captions.vtt" srclang="en" label="English" />
          Your browser does not support the audio element.
        </audio>
    |}]
;;

let%expect_test "Video player with tracks and fallback" =
  test
    {|
    <video width="320" height="240" controls poster="poster.jpg">
      <source src="movie.mp4" type="video/mp4" />
      <source src="movie.webm" type="video/webm" />
      <track kind="subtitles" src="subtitles_en.vtt" srclang="en" label="English" />
      <track kind="subtitles" src="subtitles_es.vtt" srclang="es" label="Español" />
      Your browser does not support the video tag.
    </video>
  |};
  [%expect
    {|
    No diff detected between minified input and output

    ===========OUTPUT BELOW=============

    <video width="320" height="240" controls poster="poster.jpg">
          <source src="movie.mp4" type="video/mp4" />
          <source src="movie.webm" type="video/webm" />
          <track kind="subtitles" src="subtitles_en.vtt" srclang="en" label="English" />
          <track kind="subtitles" src="subtitles_es.vtt" srclang="es" label="Español" />
          Your browser does not support the video tag.
        </video>
    |}]
;;

let%expect_test "Detailed description list with nested elements" =
  test
    {|
    <dl>
      <dt>HTML</dt>
      <dd>
        <p>Hypertext Markup Language</p>
        <ul>
          <li>Used for structuring web content</li>
          <li>Latest version: HTML5</li>
        </ul>
      </dd>
      <dt>CSS</dt>
      <dd>
        <p>Cascading Style Sheets</p>
        <ul>
          <li>Used for styling web pages</li>
          <li>Latest version: CSS3</li>
        </ul>
      </dd>
    </dl>
  |};
  [%expect
    {|
    No diff detected between minified input and output

    ===========OUTPUT BELOW=============

    <dl>
          <dt>HTML</dt>
          <dd>
            <p>Hypertext Markup Language</p>
            <ul>
              <li>Used for structuring web content</li>
              <li>Latest version: HTML5</li>
            </ul>
          </dd>
          <dt>CSS</dt>
          <dd>
            <p>Cascading Style Sheets</p>
            <ul>
              <li>Used for styling web pages</li>
              <li>Latest version: CSS3</li>
            </ul>
          </dd>
        </dl>
    |}]
;;

let%expect_test "Complex nested structure with semantic elements" =
  test
    {|
    <div class="wrapper">
      <header role="banner">
        <h1>Website Title</h1>
        <nav role="navigation" aria-label="Main navigation">
          <ul>
            <li><a href="#" aria-current="page">Home</a></li>
            <li><a href="#">About</a></li>
            <li><a href="#">Contact</a></li>
          </ul>
        </nav>
      </header>
      <main role="main">
        <article itemscope itemtype="http://schema.org/Article">
          <h2 itemprop="headline">Article Title</h2>
          <p itemprop="description">Article content goes here.</p>
          <figure>
            <img src="image.jpg" alt="Figure image" itemprop="image" />
            <figcaption itemprop="caption">Figure caption</figcaption>
          </figure>
        </article>
        <aside role="complementary">
          <h3>Related Links</h3>
          <ul>
            <li><a href="#" rel="nofollow">Link 1</a></li>
            <li><a href="#" rel="nofollow">Link 2</a></li>
          </ul>
        </aside>
      </main>
      <footer role="contentinfo">
        <p>© 2023 Your Company</p>
      </footer>
    </div>
  |};
  [%expect
    {|
    No diff detected between minified input and output

    ===========OUTPUT BELOW=============

    <div class="wrapper">
          <header role="banner">
            <h1>Website Title</h1>
            <nav role="navigation" aria-label="Main navigation">
              <ul>
                <li><a href="#" aria-current="page">Home</a></li>
                <li><a href="#">About</a></li>
                <li><a href="#">Contact</a></li>
              </ul>
            </nav>
          </header>
          <main role="main">
            <article itemscope itemtype="http://schema.org/Article">
              <h2 itemprop="headline">Article Title</h2>
              <p itemprop="description">Article content goes here.</p>
              <figure>
                <img src="image.jpg" alt="Figure image" itemprop="image" />
                <figcaption itemprop="caption">Figure caption</figcaption>
              </figure>
            </article>
            <aside role="complementary">
              <h3>Related Links</h3>
              <ul>
                <li><a href="#" rel="nofollow">Link 1</a></li>
                <li><a href="#" rel="nofollow">Link 2</a></li>
              </ul>
            </aside>
          </main>
          <footer role="contentinfo">
            <p>© 2023 Your Company</p>
          </footer>
        </div>
    |}]
;;

let%expect_test "Form with various input types and advanced attributes" =
  test
    {|
    <form action="/submit" method="post" enctype="multipart/form-data">
      <fieldset>
        <legend>User Information</legend>
        <label for="name">Name:</label>
        <input type="text" id="name" name="name" required autocomplete="name" />

        <label for="email">Email:</label>
        <input type="email" id="email" name="email" required autocomplete="email" />

        <label for="birthday">Birthday:</label>
        <input type="date" id="birthday" name="birthday" min="1900-01-01" max="2023-12-31" />

        <label for="age">Age:</label>
        <input type="number" id="age" name="age" min="0" max="120" step="1" />

        <label for="favorite-color">Favorite Color:</label>
        <input type="color" id="favorite-color" name="favorite-color" />

        <fieldset>
          <legend>Gender</legend>
          <input type="radio" id="male" name="gender" value="male" />
          <label for="male">Male</label>
          <input type="radio" id="female" name="gender" value="female" />
          <label for="female">Female</label>
          <input type="radio" id="other" name="gender" value="other" />
          <label for="other">Other</label>
        </fieldset>

        <label for="interests">Interests:</label>
        <select id="interests" name="interests" multiple size="4">
          <option value="sports">Sports</option>
          <option value="music">Music</option>
          <option value="reading">Reading</option>
          <option value="travel">Travel</option>
        </select>

        <label for="profile-picture">Profile Picture:</label>
        <input type="file" id="profile-picture" name="profile-picture" accept="image/*" />

        <label for="bio">Bio:</label>
        <textarea id="bio" name="bio" rows="4" cols="50" maxlength="500" placeholder="Tell us about yourself..." ></textarea>

        <label for="website">Website:</label>
        <input type="url" id="website" name="website" placeholder="https://example.com" />

        <label for="rating">Rate your experience (1-5):</label>
        <input type="range" id="rating" name="rating" min="1" max="5" step="1" list="ratings" />
        <datalist id="ratings">
          <option value="1" label="1"></option>
          <option value="2"></option>
          <option value="3"></option>
          <option value="4"></option>
          <option value="5" label="5"></option>
        </datalist>

        <label for="subscribe">
          <input type="checkbox" id="subscribe" name="subscribe" value="yes" />
          Subscribe to newsletter
        </label>

        <input type="hidden" name="form-id" value="user-registration" />

        <button type="submit">Submit</button>
        <button type="reset">Reset</button>
      </fieldset>
    </form>
  |};
  [%expect
    {|
    No diff detected between minified input and output

    ===========OUTPUT BELOW=============

    <form action="/submit" method="post" enctype="multipart/form-data">
          <fieldset>
            <legend>User Information</legend>
            <label for="name">Name:</label>
            <input type="text" id="name" name="name" required autocomplete="name" />

            <label for="email">Email:</label>
            <input type="email" id="email" name="email" required autocomplete="email" />

            <label for="birthday">Birthday:</label>
            <input type="date" id="birthday" name="birthday" min="1900-01-01" max="2023-12-31" />

            <label for="age">Age:</label>
            <input type="number" id="age" name="age" min="0" max="120" step="1" />

            <label for="favorite-color">Favorite Color:</label>
            <input type="color" id="favorite-color" name="favorite-color" />

            <fieldset>
              <legend>Gender</legend>
              <input type="radio" id="male" name="gender" value="male" />
              <label for="male">Male</label>
              <input type="radio" id="female" name="gender" value="female" />
              <label for="female">Female</label>
              <input type="radio" id="other" name="gender" value="other" />
              <label for="other">Other</label>
            </fieldset>

            <label for="interests">Interests:</label>
            <select id="interests" name="interests" multiple size="4">
              <option value="sports">Sports</option>
              <option value="music">Music</option>
              <option value="reading">Reading</option>
              <option value="travel">Travel</option>
            </select>

            <label for="profile-picture">Profile Picture:</label>
            <input type="file" id="profile-picture" name="profile-picture" accept="image/*" />

            <label for="bio">Bio:</label>
            <textarea id="bio" name="bio" rows="4" cols="50" maxlength="500" placeholder="Tell us about yourself..."></textarea>

            <label for="website">Website:</label>
            <input type="url" id="website" name="website" placeholder="https://example.com" />

            <label for="rating">Rate your experience (1-5):</label>
            <input type="range" id="rating" name="rating" min="1" max="5" step="1" list="ratings" />
            <datalist id="ratings">
              <option value="1" label="1"></option>
              <option value="2"></option>
              <option value="3"></option>
              <option value="4"></option>
              <option value="5" label="5"></option>
            </datalist>

            <label for="subscribe">
              <input type="checkbox" id="subscribe" name="subscribe" value="yes" />
              Subscribe to newsletter
            </label>

            <input type="hidden" name="form-id" value="user-registration" />

            <button type="submit">Submit</button>
            <button type="reset">Reset</button>
          </fieldset>
        </form>
    |}]
;;

let%expect_test "svg" =
  test
    {|<svg width="200" height="200" viewBox="0 0 200 200">
        <circle cx="100" cy="100" r="80" fill="blue" />
        <rect x="60" y="60" width="80" height="80" fill="green" />
        <polygon points="100,40 60,160 140,160" fill="red" />
    </svg>|};
  [%expect
    {|
    No diff detected between minified input and output

    ===========OUTPUT BELOW=============

    <svg width="200" height="200" viewBox="0 0 200 200">
            <circle cx="100" cy="100" r="80" fill="blue" />
            <rect x="60" y="60" width="80" height="80" fill="green" />
            <polygon points="100,40 60,160 140,160" fill="red" />
        </svg>
    |}]
;;

let%expect_test "comment doesn't crash" =
  test {|<div>Hello! <!--comment --> </div>|};
  [%expect
    {|
    ####### DIFF -INPUT +OUTPUT ########

    -1,1 +1,1
    -|<div>Hello! <!--comment --> </div>
    +|<div>Hello!  </div>

    ===========OUTPUT BELOW=============

    <div>Hello!  </div>
    |}]
;;
