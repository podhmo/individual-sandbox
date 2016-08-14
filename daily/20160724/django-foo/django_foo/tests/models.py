from django.db import models


class User(models.Model):
    name = models.CharField(max_length=32, null=False, blank=True, default="")


class Group(models.Model):
    name = models.CharField(max_length=32, null=False, blank=True, default="")
    users = models.ManyToManyField(User, related_name="groups")
