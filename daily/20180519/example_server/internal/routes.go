package internal

func (s *server) routes() {
	s.router.HandleFunc("/about", s.handleAbout())
	s.router.HandleFunc("/greet", s.APIOnly(s.handleGreeting("Hello %s")))
}
