import social.backends.base as base


class UserMixin(base.UserMixin):
    @classmethod
    def changed(cls, user):
        """The given user instance is ready to be saved"""
        raise NotImplementedError('Implement in subclass')

    @classmethod
    def get_username(cls, user):
        """Return the username for given user"""
        raise NotImplementedError('Implement in subclass')

    @classmethod
    def user_model(cls):
        """Return the user model"""
        raise NotImplementedError('Implement in subclass')

    @classmethod
    def username_max_length(cls):
        """Return the max length for username"""
        raise NotImplementedError('Implement in subclass')

    @classmethod
    def allowed_to_disconnect(cls, user, backend_name, association_id=None):
        """Return if it's safe to disconnect the social account for the
        given user"""
        raise NotImplementedError('Implement in subclass')

    @classmethod
    def disconnect(cls, entry):
        """Disconnect the social account for the given user"""
        raise NotImplementedError('Implement in subclass')

    @classmethod
    def user_exists(cls, *args, **kwargs):
        """
        Return True/False if a User instance exists with the given arguments.
        Arguments are directly passed to filter() manager method.
        """
        raise NotImplementedError('Implement in subclass')

    @classmethod
    def create_user(cls, *args, **kwargs):
        """Create a user instance"""
        raise NotImplementedError('Implement in subclass')

    @classmethod
    def get_user(cls, pk):
        """Return user instance for given id"""
        raise NotImplementedError('Implement in subclass')

    @classmethod
    def get_users_by_email(cls, email):
        """Return users instances for given email address"""
        raise NotImplementedError('Implement in subclass')

    @classmethod
    def get_social_auth(cls, provider, uid):
        """Return UserSocialAuth for given provider and uid"""
        raise NotImplementedError('Implement in subclass')

    @classmethod
    def get_social_auth_for_user(cls, user, provider=None, id=None):
        """Return all the UserSocialAuth instances for given user"""
        raise NotImplementedError('Implement in subclass')

    @classmethod
    def create_social_auth(cls, user, uid, provider):
        """Create a UserSocialAuth instance for given user"""
        raise NotImplementedError('Implement in subclass')


class NonceMixin(base.NonceMixin):
    @classmethod
    def use(cls, server_url, timestamp, salt):
        """Create a Nonce instance"""
        raise NotImplementedError('Implement in subclass')


class AssociationMixin(base.AssociationMixin):
    @classmethod
    def store(cls, server_url, association):
        """Create an Association instance"""
        raise NotImplementedError('Implement in subclass')

    @classmethod
    def get(cls, *args, **kwargs):
        """Get an Association instance"""
        raise NotImplementedError('Implement in subclass')

    @classmethod
    def remove(cls, ids_to_delete):
        """Remove an Association instance"""
        raise NotImplementedError('Implement in subclass')


class CodeMixin(base.CodeMixin):
    @classmethod
    def get_code(cls, code):
        raise NotImplementedError('Implement in subclass')


class BaseStorage(object):
    user = UserMixin
    nonce = NonceMixin
    association = AssociationMixin
    code = CodeMixin

    @classmethod
    def is_integrity_error(cls, exception):
        """Check if given exception flags an integrity error in the DB"""
        raise NotImplementedError('Implement in subclass')
