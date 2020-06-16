query RebelsQuery {
  rebels {
    name,
    ships(first: 1) {
      edges {
        cursor,
        node {
          name
        }
      }
    }
  }
}
