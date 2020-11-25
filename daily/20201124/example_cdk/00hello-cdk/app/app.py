#!/usr/bin/env python3

from aws_cdk import core

from app.app_stack import AppStack


app = core.App()
AppStack(app, "app")

app.synth()
