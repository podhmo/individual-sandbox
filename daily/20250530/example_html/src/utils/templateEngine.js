export const processTemplate = (template, values) => {
  if (typeof template !== 'string') return '';
  let result = template;
  for (const key in values) {
    if (Object.hasOwnProperty.call(values, key)) {
      const regex = new RegExp(`\\{\\{\\s*${key}\\s*\\}\\}`, 'g');
      result = result.replace(regex, values[key]);
    }
  }
  return result;
};