from django.test import TestCase


class ModelTests(TestCase):
    @classmethod
    def setUpTestData(cls):
        from .models import User, Group

        users = User.objects.bulk_create([
            User(name="foo"),
            User(name="bar"),
            User(name="boo"),
        ])
        groups = Group.objects.bulk_create([
            Group(name="A"),
            Group(name="B"),
            Group(name="C"),
        ])
        cls.users = users = list(User.objects.all())
        cls.groups = groups = list(Group.objects.all())

        groups[0].users.add(users[0])
        groups[0].users.add(users[1])
        groups[0].users.add(users[2])
        groups[1].users.add(users[1])
        groups[1].users.add(users[2])
        groups[2].users.add(users[2])

    def test_query_group(self):
        from .models import Group
        candidates = [
            ("foo", 1),
            ("bar", 2),
            ("boo", 3),
        ]
        for user_name, expected_count in candidates:
            with self.subTest(user_name=user_name):
                qs = Group.objects.filter(users__name=user_name)
                self.assertEqual(qs.count(), expected_count)

    def test_query_user(self):
        from .models import User
        candidates = [
            ("A", 3),
            ("B", 2),
            ("C", 1),
        ]
        for group_name, expected_count in candidates:
            with self.subTest(group_name=group_name):
                qs = User.objects.filter(groups__name=group_name)
                self.assertEqual(qs.count(), expected_count)

    def test_query_without_prefetch(self):
        from .models import User
        qs = User.objects.order_by("id")

        with self.assertNumQueries(4):
            result = []
            for user in qs:
                result.append((user.name, len(user.groups.all())))
        expected = [
            ('foo', 1), ("bar", 2), ("boo", 3)
        ]
        self.assertEqual(result, expected)

    def test_query_with_prefetch(self):
        from .models import User
        qs = User.objects.order_by("id")

        with self.assertNumQueries(2):
            result = []
            for user in qs.prefetch_related("groups"):
                result.append((user.name, len(user.groups.all())))
        expected = [
            ('foo', 1), ("bar", 2), ("boo", 3)
        ]
        self.assertEqual(result, expected)
