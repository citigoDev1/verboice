class OmniauthCallbacksController < Devise::OmniauthCallbacksController
  skip_before_filter :verify_authenticity_token
  skip_before_filter :check_guisso_cookie

  def instedd
    generic do |auth|
      {
        email: auth.info['email'],
        # name: auth.info['name'],
      }
    end
  end

  def generic
    auth = env['omniauth.auth']

    if identity = Identity.find_by_provider_and_token(auth['provider'], auth['uid'])
      account = identity.account
    else
      attributes = yield auth

      attributes[:confirmed_at] = Time.now

      account = Account.find_by_email(attributes[:email])
      unless account
        password = Devise.friendly_token
        account = Account.create!(attributes.merge(password: password, password_confirmation: password))
      end
      account.identities.create! provider: auth['provider'], token: auth['uid']
    end

    sign_in account
    next_url = env['omniauth.origin'] || root_path
    next_url = root_path if next_url == new_account_session_url
    redirect_to next_url
  end
end
