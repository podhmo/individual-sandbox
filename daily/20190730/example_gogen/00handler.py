def handler(request):
    if not is_authenticated(request):
        raise Unauthorized("401")

    session = request.session
    user = get_user(session, requst.params["user_id"])
    target = get_target(requst.session, request.params["id"])

    if not is_authorized(target, user=usr):
        raise Forbidden(403)

    params = paramterize(request.params)
    result = do_something(user, target, params)
    return model_to_dict(result)
