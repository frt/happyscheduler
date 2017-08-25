import { Component } from '@angular/core';
import { PushNotificationsService, SimpleNotificationsModule, NotificationsService } from 'angular2-notifications';

@Component({
    selector: 'app-root',
    templateUrl: './app.component.html',
    styleUrls: ['./app.component.css']
})
export class AppComponent {

    constructor
        ( private _pushNotifications: PushNotificationsService
        ) {
        _pushNotifications.requestPermission();
    }
}
