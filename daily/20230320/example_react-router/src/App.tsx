import React from "react";
import ReactDOM from "react-dom";
import {
  BrowserRouter as Router,
  Routes,
  Route,
  Link,
  Outlet,
  useParams
} from "react-router-dom";

export default function App() {
  return (
    <Router>
      <div>
        <nav>
          <Link to="/">Home</Link> | <Link to="about">About</Link> |{" "}
          <Link to="blog">Blog</Link>
        </nav>
        <hr />
        <Routes>
          <Route path="/" element={<Home />} />
          <Route path="about" element={<About />} />
          <Route path="blog" element={<Blog />} />
          <Route path="blog/:slug" element={<Post />} />
        </Routes>
      </div>
    </Router>
  );
}

function Home() {
  return (
    <div>
      <h1>Home</h1>
      <p>Welcome to the home page!</p>
    </div>
  );
}

function About() {
  return (
    <div>
      <h1>About</h1>
      <p>This is a demo of react-router without remix.</p>
    </div>
  );
}

function Blog() {
  return (
    <div>
      <h1>Blog</h1>
      <ul>
        <li>
          <Link to="hello-world">Hello World</Link>
        </li>
        <li>
          <Link to="another-post">Another Post</Link>
        </li>
        <li>
          <Link to="yet-another-post">Yet Another Post</Link>
        </li>
      </ul>

      {/* This element renders the element for the child route */}
      <Outlet />
    </div>
  );
}

function Post() {
  // The `slug` param is available from useParams
  let { slug } = useParams();

  return (
    <div>
      <h2>{slug}</h2>
      <p>This is the post content.</p>
    </div>
  );
}

