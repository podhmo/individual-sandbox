from marshmallow import Schema, fields, ValidationError, validates_schema


class S(Schema):
    state = fields.Raw()

    @validates_schema
    def validate_analysis_status(self, data):
        if "state" in data:
            if data["state"].get("status") == "waiting":
                raise ValidationError('analysis status must be after processing status', field_names=["status"])

try:
    print(S(strict=True).load({"state": {"status": "waiting"}}))
except Exception as e:
    print(vars(e))
